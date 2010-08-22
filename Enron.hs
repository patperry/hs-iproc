module Enron (
    Employee(..),
    Gender(..),
    Seniority(..),
    Department(..),
    
    Email(..),
    
    fetchEmployeeList,
    fetchEmailList,
    ) where
        
import Control.Applicative
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Database.HDBC


type EmployeeId = Int
data Gender = Female | Male deriving (Eq, Show, Read)
data Seniority = Junior | Senior deriving (Eq, Show, Read)
data Department = Legal | Trading | Other deriving (Eq, Show, Read)

data Employee =
    Employee { employeeId :: !EmployeeId
             , employeeName :: !String
             , employeeLongDepartment :: !String
             , employeeTitle :: !String
             , employeeGender :: !Gender
             , employeeSeniority :: !Seniority
             , employeeDepartment :: !Department
             }
        deriving (Eq, Show)

type EmailId = Int
data Email =
    Email { emailId :: !EmailId
          , emailFilename :: !String
          , emailTime :: !UTCTime
          , emailSubject :: !String
          , emailFrom :: !EmployeeId
          , emailToList :: ![EmployeeId]
          }
        deriving (Eq, Show)

fetchEmployeeList :: (IConnection conn) => conn -> IO [Employee]
fetchEmployeeList conn = do
    map parseEmployee <$>
        quickQuery conn (
            " SELECT\
            \     eid,\
            \     name,\
            \     longdepartment,\
            \     title,\
            \     gender,\
            \     seniority,\
            \     department\
            \ FROM\
            \     Employee\
            \ ") []
  where
    parseEmployee [ eid
                  , name
                  , longdepartment
                  , title
                  , gender
                  , seniority
                  , department
                  ] =
        Employee { employeeId = fromSql eid
                 , employeeName = fromSql name
                 , employeeLongDepartment = fromSql longdepartment
                 , employeeTitle = fromSql title
                 , employeeGender = read $ fromSql gender
                 , employeeSeniority = read $ fromSql seniority
                 , employeeDepartment = read $ fromSql department
                 }
    parseEmployee _ = error "parseEmployee: pattern match failure"                 


fetchEmailList :: (IConnection conn) => conn -> IO [Email]
fetchEmailList conn = do
    msgs <- aggregate <$>
                quickQuery conn (
                    " SELECT\
                    \     R.mid,\
                    \     R.to_eid,\
                    \     M.filename,\
                    \     M.unix_time,\
                    \     M.subject,\
                    \     M.from_eid\
                    \ FROM\
                    \     Message M,\
                    \     Recipient R\
                    \ WHERE\
                    \     M.mid = R.mid\
                    \ ORDER BY\
                    \     M.unix_time\
                    \ ") []
    return msgs
  where
    aggregate [] = []
    aggregate (row:rows) = aggregateWith (parseEmail row) rows
    
    aggregateWith e [] = [e]
    aggregateWith e raw@(row:rows) = case row of
        mid:t:_ | emailId e == fromSql mid ->
                      let e' = e{ emailToList = emailToList e ++ [ fromSql t]}
                      in aggregateWith e' rows
        _ -> e:(aggregate raw)

    parseEmail [ mid
               , to_eid
               , filename
               , unix_time
               , subject
               , from_eid
               ] =
        Email { emailId = fromSql mid
              , emailFilename = fromSql filename
              , emailTime = posixSecondsToUTCTime $ fromSql unix_time
              , emailSubject = fromSql subject
              , emailFrom = fromSql from_eid
              , emailToList = [fromSql to_eid]
              }
    parseEmail _ = error "parseEmail: pattern match failure"

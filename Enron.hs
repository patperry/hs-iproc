module Enron (
    Employee(..),
    Gender(..),
    Seniority(..),
    Department(..),
    
    Email(..),
    
    fetchEmployeeList',
    fetchEmailList',
    ) where
        
import Control.Applicative
import Database.HDBC

import Data.Map( Map )
import qualified Data.Map as Map

import Data.Time.Clock
import Data.Time.Clock.POSIX


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
          , emaileFilename :: !String
          , emailTime :: !UTCTime
          , emailSubject :: !String
          , emailFrom :: !EmployeeId
          , emailToList :: ![EmployeeId]
          }
        deriving (Eq, Show)

fetchEmployeeList' :: (IConnection conn) => conn -> IO [Employee]
fetchEmployeeList' conn = do
    map parseEmployee <$>
        quickQuery' conn (
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
        Employee (fromSql eid)
                 (fromSql name)
                 (fromSql longdepartment)
                 (fromSql title)
                 (read $ fromSql gender)
                 (read $ fromSql seniority)
                 (read $ fromSql department)

fetchRecipientMap' :: (IConnection conn)
                   => conn -> IO (Map EmailId [EmployeeId])
fetchRecipientMap' conn = do
    rcps <- map parseRecipient  <$>
                quickQuery' conn (
                    " SELECT\
                    \     mid,\
                    \     to_eid\
                    \ FROM\
                    \     Recipient\
                    \ ") []
    return $ foldr (\(mid,to) -> to `seq` Map.insertWith' (++) mid [to])
                   Map.empty rcps
  where
    parseRecipient [ mid, to_eid ] =
        (fromSql mid, fromSql to_eid)

fetchEmailList' :: (IConnection conn)
                => conn -> IO [Email]
fetchEmailList' conn = do
    rmap <- fetchRecipientMap' conn
    let tos mid = Map.findWithDefault [] mid rmap
                
    msgs <- map (parseEmail tos) <$>
                quickQuery' conn (
                    " SELECT\
                    \     mid,\
                    \     filename,\
                    \     unix_time,\
                    \     subject,\
                    \     from_eid\
                    \ FROM\
                    \     Message\
                    \ ") []
    return msgs
  where
    parseEmail tos [ mid
                   , filename
                   , unix_time
                   , subject
                   , from_eid
                   ] =
        Email (fromSql mid)
              (fromSql filename)
              (posixSecondsToUTCTime $ fromSql unix_time)
              (fromSql subject)
              (fromSql from_eid)
              (tos $ fromSql mid)

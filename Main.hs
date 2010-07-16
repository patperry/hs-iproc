module Main
    where
        
main = do
    conn <- connectSqlite3 "enron.db"
    emps <- fetchEmployeeList conn
    msgs <- fetchMessageList conn
    disconnect conn
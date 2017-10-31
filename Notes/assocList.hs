type Dictionary = [(String, Integer)]

look :: Dictionary -> String -> Maybe Integer
look [] str = Nothing
look ((key,val): dict) str =
    if key == str
        then Just val
        else look dict str

update :: Dictionary -> String -> Integer -> Dictionary
update [] str n = [(str, n)]
update ((key,val):dict) str n = 
    if key == str
        then (key,n):dict
        else update dict str n

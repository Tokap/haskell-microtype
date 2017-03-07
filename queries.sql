-- Get traversal to be initialized (run traversal on it)
SELECT T.id, T.network_account_id
FROM traversal T
WHERE T.history_completed IS NULL
       AND (processor_uuid IS NULL OR traversal_status_id = 2)
ORDER BY created asc
LIMIT 1;

-- Get network_account by id (including network_type_code)
SELECT NA.*, NT.code network_type_code
FROM network_account NA
  LEFT JOIN network_type NT ON NA.network_type_id = NT.id
WHERE NA.id = ?;



------- ME BUILDING A QUERY:
SELECT *
FROM traversal AS T
LEFT JOIN network_account AS NA
ON T.network_account_id=NA.id
LEFT JOIN network_type AS NT
ON NA.network_type_id=NT.id
WHERE `history_completed` IS NULL
AND `processor_uuid` IS NULL;

------- Clear Results
UPDATE traversal
SET last_updated=NULL,
	history_completed=NULL,
	traversal_status_id=0,
	processor_uuid=NULL
WHERE id IN (1,2,3,4,5,6)

&&

truncate network_post_data;

-------- Check and update max connections (I was breaking this)
show variables like "max_connections";
set global max_connections = 200;

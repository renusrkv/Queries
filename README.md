SELECT 'alter system kill session ''' || b.sid || ', ' || b.serial# || ''';'
  FROM DBA_DDL_LOCKS a, v$session b
WHERE 1 = 1 AND a.session_id = b.sid AND a.name = '';

 SELECT SID,INST_ID FROM gv$access WHERE OBJECT= upper('');

 SELECT 'alter system kill session '''||SID||','||serial#||',@'|| inst_id || '''' || ' IMMEDIATE' || ';' FROM gv$session WHERE SID IN (SELECT UNIQUE SID FROM gv$access WHERE OBJECT= upper(''));

c:
cd \
cd \_R_PACKAGES\easydb\preparation\databases 
DEL soils2.db  
sqlite3 soils2.db ".databases" 
sqlite3 soils2.db "PRAGMA foreign_keys = ON" 
sqlite3 soils2.db ".read create_SQLite_db.sql" 
sqlite3 soils2.db ".tables" 
pause 


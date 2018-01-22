-- Add NBNUser back to dbo role since Application Roles don't seem to play nicely with ADO

EXEC sp_addrolemember 'db_Owner', 'NBNUser'

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SystemSuppliedData_BCP_Import]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SystemSuppliedData_BCP_Import]
GO

/*===========================================================================*\
  Description:	Imports system supplied data from text files located in the 
		given location.

  Parameters:	@InputPath	Location for input files.

  Created:	September 2003

  Last revision information:
    $Revision: 2 $
    $Date: 9/01/06 13:42 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SystemSuppliedData_BCP_Import]
	@InputPath varchar(1000) = NULL
AS

	DECLARE @TableName varchar(50),
		@Cmd varchar(2000),
		@PK1 sysname,
		@PK2 sysname,
		@PKJoins varchar(500),
		@PKField varchar(100),
		@Exists int,
		@TablePath varchar(1000),
		@field varchar(100),
		@fieldlist varchar(2000),
		@fieldlistwithalias varchar(2000)


	/*-------------------------------------------------------------*\
	  Set default output path, if none provided.
	\*-------------------------------------------------------------*/
	IF @InputPath IS NULL
		SET @InputPath = 'C:\Temp\'
	
	/*-------------------------------------------------------------*\
	  Declare a cursor to loop through all the tables with a
	  System Supplied Data field.
	\*-------------------------------------------------------------*/
	DECLARE curTableNames CURSOR LOCAL FAST_FORWARD
	FOR
		SELECT	DISTINCT	TableName = Convert(SysName, O.Name)
		FROM		SysObjects O
		INNER JOIN 	SysColumns C ON C.Id = O.Id
		WHERE		O.Type = 'U'
		AND		C.Number = 0
		AND		(C.Name = 'System_Supplied_Data' OR O.Name IN ('Meaning', 'Language', 'Store', 'Collection_Unit', 'Database_Relationship','Macro'))
		ORDER BY 	Convert(SysName, O.Name)

	/*-------------------------------------------------------------*\
	  Run BCP command on each table returned.
	\*-------------------------------------------------------------*/
	OPEN curTableNames
	FETCH NEXT FROM curTableNames INTO @TableName

	WHILE @@Fetch_Status = 0
	BEGIN
		-- Test we have data to import, if not, skip the table
		SET @TablePath = @InputPath + @TableName + '.txt'
		execute master.dbo.xp_fileexist @fileName = @TablePath, @Exists = @Exists output
		IF @Exists=1 BEGIN
			--Create a temp table to hold the data
			SET @Cmd = 'SELECT TOP 0 * INTO ImportTemp FROM ' + @TableName
			EXEC (@Cmd)
	
			SET @Cmd = 'bcp ' + DB_Name() + '.dbo.ImportTemp in "' + 
				@InputPath + @TableName + '.txt" -m50 -T -c -t*@*@ -r!@!@ -S' + @@ServerName
	
			EXEC Master..xp_CmdShell @Cmd, no_output

			-- Identify the list of columns for the table.
			DECLARE curImportFields CURSOR FOR
				select [name] from sysColumns where id=object_id(@TableName) and [name]<>'timestamp'
			OPEN curImportFields
			SET @fieldlist=''
			SET @fieldlistwithalias=''

			WHILE 1=1
			BEGIN
				FETCH NEXT FROM curImportFields INTO @field
				IF @@Fetch_Status<>0 BREAK
				IF @fieldlist<>'' 
				BEGIN
					SET @fieldlist = @fieldlist + ', '
					SET @fieldlistwithalias = @fieldlistwithalias + ', '
				END
				SET @fieldlist = @fieldlist + @field
				SET @fieldlistwithalias = @fieldlistwithalias + 'T1.' + @field
			END
			
			CLOSE curImportFields
			DEALLOCATE curImportFields			
	
			-- Identify primary key fields for table		
			DECLARE curKeyFields CURSOR FOR
				select kcu.COLUMN_NAME 
				from INFORMATION_SCHEMA.TABLE_CONSTRAINTS as tc 
				join INFORMATION_SCHEMA.KEY_COLUMN_USAGE as kcu 
				  on (tc.TABLE_CATALOG = kcu.TABLE_CATALOG and 
				         tc.TABLE_SCHEMA  = kcu.TABLE_SCHEMA and 
				         tc.TABLE_NAME    = kcu.TABLE_NAME and 
				         tc.CONSTRAINT_NAME = kcu.CONSTRAINT_NAME) 
				where tc.TABLE_CATALOG   = DB_Name() 
				and   tc.TABLE_SCHEMA    = 'dbo' 
				and   tc.TABLE_NAME      = @TableName
				and   tc.CONSTRAINT_TYPE = 'PRIMARY KEY' 
				order by kcu.ORDINAL_POSITION 

			SET @PKJoins='LEFT JOIN '+@TableName + ' T2'
	
			OPEN curKeyFields
	
			FETCH NEXT FROM curKeyFields INTO @PKField
			IF @@Fetch_Status=0
			BEGIN
				SET @PKJoins = @PKJoins + ' ON T2.' + @PKField + '=T1.' + @PkField
	
				WHILE @@Fetch_Status=0
				BEGIN
					FETCH NEXT FROM curKeyFields INTO @PKField			
					IF @@Fetch_Status=0
						SET @PKJoins = @PKJoins + ' AND T2.' + @PKField + '=T1.' + @PkField
				END
		
				-- Copy across data that does not already exist
				SET @Cmd = 'INSERT INTO ' + @TableName + '(' + @fieldlist + ')' +
					' SELECT ' + @fieldlistwithalias + ' FROM ImportTemp T1 ' + @PKJoins +
					' WHERE T2.' + @PkField + ' IS NULL '
			END
			ELSE
				-- No primary key yet, so must be a new table, no need to check for existing data
				SET @Cmd = 'INSERT INTO ' + @TableName + '(' + @fieldlist + ')' +
						' SELECT ' + @fieldlist + ' FROM ImportTemp'
			EXEC (@Cmd)

			CLOSE curKeyFields
			DEALLOCATE curKeyFields
						
			DROP TABLE ImportTemp
		END
		FETCH NEXT FROM curTableNames INTO @TableName
	END

	/*-------------------------------------------------------------*\
	  Cleanup.
	\*-------------------------------------------------------------*/
	CLOSE curTableNames
	DEALLOCATE curTableNames
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
GRANT EXECUTE ON [dbo].[usp_SystemSuppliedData_BCP_Import] TO [Public]
GO
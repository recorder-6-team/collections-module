/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SystemSuppliedData_BCP_Export]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SystemSuppliedData_BCP_Export]
GO

/*===========================================================================*\
  Description:	Exports system supplied data as text files to the given
		output location.

  Parameters:	@OutputPath	Location for output files.

  Created:	September 2003

  Last revision information:
    $Revision: 10 $
    $Date: 12/05/04 9:57 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SystemSuppliedData_BCP_Export]
	@OutputPath varchar(1000) = NULL
AS
	DECLARE @TableName varchar(50),
		@Cmd varchar(500),
		@PrimaryKey varchar(50)

	/*-------------------------------------------------------------*\
		Ensure developer's domains do not transfer through
	\*-------------------------------------------------------------*/
  UPDATE Collection_Unit SET domain_Mask=0 WHERE collection_Unit_Key like 'SYSTEM00%'

	/*-------------------------------------------------------------*\
	  Set default output path, if none provided.
	\*-------------------------------------------------------------*/
	IF @OutputPath IS NULL
		SET @OutputPath = 'C:\Temp\'
	
	/*-------------------------------------------------------------*\
	  Declare a cursor to loop through all the tables with a
	  System Supplied Data field.
	\*-------------------------------------------------------------*/
	DECLARE curTableNames CURSOR LOCAL FAST_FORWARD
	FOR
		SELECT		TableName = Convert(SysName, O.Name)
		FROM		SysObjects O
		INNER JOIN 	SysColumns C ON C.Id = O.Id
		WHERE		O.Type = 'U'
		AND		C.Number = 0
		AND		(C.Name = 'System_Supplied_Data' OR O.NAME IN ('Macro', 'Meaning', 'Language', 'Store', 'Collection_Unit', 'Database_Relationship'))
		ORDER BY 	O.Name

	/*-------------------------------------------------------------*\
	  Run BCP command on each table returned.
	\*-------------------------------------------------------------*/
	OPEN curTableNames
	FETCH NEXT FROM curTableNames INTO @TableName

  WHILE @@Fetch_Status = 0
	BEGIN
		--Obtain table's primary key 
		SELECT @PrimaryKey = KCU.Column_Name
		FROM Information_Schema.Table_Constraints AS TC
		INNER JOIN Information_Schema.Key_Column_Usage AS KCU
		  ON (TC.Table_Catalog = kcu.Table_Catalog and
		         TC.Table_Schema  = kcu.Table_Schema and
		         TC.Table_Name    = kcu.Table_Name and
		         TC.Constraint_Name = kcu.Constraint_Name)
		WHERE TC.Table_Catalog   = 'CollectionsDev'
		AND   TC.Table_Schema    = 'dbo'
		AND   TC.Table_Name      = @TableName
		AND   TC.Constraint_Type = 'PRIMARY KEY'

		IF @TableName IN ('Meaning', 'Store', 'Collection_Unit', 'Database_Relationship', 'Macro')
			SET @Cmd = 'bcp "SELECT * FROM ' + DB_Name() + '.dbo.' + @TableName + ' WHERE ' +
					'LEFT(' + @PrimaryKey + ', 8)=''SYSTEM00''" ' +
			 	 'queryout "' + @OutputPath + @TableName + '.txt" -m50 -T -c -t*@*@ -r!@!@ -S' + @@ServerName
		ELSE IF @TableName = 'Language'
			SET @Cmd = 'bcp "SELECT * FROM ' + DB_Name() + '.dbo.' + @TableName + 
				 '" queryout "' + @OutputPath + @TableName + '.txt" -m50 -T -c -t*@*@ -r!@!@ -S' + @@ServerName
		ELSE IF @TableName = 'Taxon_Dictionary_Name_Type_Mapping'
			SET @Cmd = 'bcp "SELECT * FROM ' + DB_Name() + '.dbo.' + @TableName + ' WHERE ' +
					'System_Supplied_Data = 1" ' +
				 'queryout "' + @OutputPath + @TableName + '.txt" -m50 -T -c -t*@*@ -r!@!@ -S' + @@ServerName
		ELSE
			SET @Cmd = 'bcp "SELECT * FROM ' + DB_Name() + '.dbo.' + @TableName + ' WHERE System_Supplied_Data = 1 AND ' +
					'LEFT(' + @PrimaryKey + ', 8)=''SYSTEM00''" ' +
			 	 'queryout "' + @OutputPath + @TableName + '.txt" -m50 -T -c -t*@*@ -r!@!@ -S' + @@ServerName

		EXEC Master..xp_CmdShell @Cmd, no_output

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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SystemSuppliedData_BCP_Export') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SystemSuppliedData_BCP_Export'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SystemSuppliedData_BCP_Export TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SystemSuppliedData_BCP_Export TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SystemSuppliedData_BCP_Export TO [Dev - JNCC SQL]
END
GO
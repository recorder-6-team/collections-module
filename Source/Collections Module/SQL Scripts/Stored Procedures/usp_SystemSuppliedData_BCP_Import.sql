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
    $Revision: 5 $
    $Date: 12/05/04 9:57 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SystemSuppliedData_BCP_Import]
	@InputPath varchar(1000) = NULL
AS
	DECLARE @TableName varchar(50),
		@Cmd varchar(500)

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
		AND		(C.Name = 'System_Supplied_Data' OR O.Name IN ('Macro', 'Meaning', 'Language', 'Store', 'Collection_Unit', 'Database_Relationship'))
		ORDER BY 	Convert(SysName, O.Name)

	/*-------------------------------------------------------------*\
	  Run BCP command on each table returned.
	\*-------------------------------------------------------------*/
	OPEN curTableNames
	FETCH NEXT FROM curTableNames INTO @TableName

	WHILE @@Fetch_Status = 0
	BEGIN
		SET @Cmd = 'bcp ' + DB_Name() + '.dbo.' + @TableName + 
			' in "' + @InputPath + @TableName + '.txt" -m50 -T -c -t*@*@ -r!@!@ -S' + @@ServerName

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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SystemSuppliedData_BCP_Import') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SystemSuppliedData_BCP_Import'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SystemSuppliedData_BCP_Import TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SystemSuppliedData_BCP_Import TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SystemSuppliedData_BCP_Import TO [Dev - JNCC SQL]
END
GO
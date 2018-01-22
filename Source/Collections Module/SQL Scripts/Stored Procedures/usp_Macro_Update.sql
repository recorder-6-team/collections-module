/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Macro_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Macro_Update]
GO

/*===========================================================================*\
  Description:	Returns the list of macros

  Created:	April 2004

  Last revision information:
    $Revision: 2 $
    $Date: 12/07/04 11:17 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Macro_Update]
	@NumberType VARCHAR(20),
	@Macro VARCHAR(200),
	@MacroIDGeneration BIT,
	@IDMacro VARCHAR(100),
	@IDSeed INT
AS
	UPDATE Macro
	SET Macro = @Macro,
		 Macro_ID_Generation = @MacroIDGeneration,
		 ID_Macro=@IDMacro,
		 ID_Seed=@IDSeed
	WHERE Number_Type=@NumberType

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Macro_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Macro_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Macro_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Macro_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Macro_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Macro_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Macro_Update TO [Dev - JNCC SQL]
END

GO

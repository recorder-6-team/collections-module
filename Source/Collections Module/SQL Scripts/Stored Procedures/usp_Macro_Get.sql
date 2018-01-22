/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Macro_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Macro_Get]
GO

/*===========================================================================*\
  Description:	Returns the next available macro value of a particular type

  Parameters: @NumberType (Registration, Accession, etc)
		@Dept - department acronym to use in macro
		@Macro OUTPUT

  Created:	April 2004

  Last revision information:
    $Revision: 5 $
    $Date: 11/08/04 11:25 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Macro_Get]
	@NumberType VARCHAR(20),
	@Dept VARCHAR(100),
	@Macro VARCHAR(200) OUTPUT

AS

DECLARE @MacroIDGeneration BIT
DECLARE @IDMacro VARCHAR(100)
DECLARE @LastID INT
DECLARE @IDSeed INT

SELECT 
		@Macro=Macro,
		@MacroIDGeneration = Macro_ID_Generation,
		@IDMacro = ID_Macro,
		@LastID = Last_Global_ID,
		@IDSeed = ID_Seed
FROM Macro
WHERE Number_Type = @NumberType

IF @Macro IS NULL 
	SET @Macro=''
ELSE
BEGIN

	SET @Macro = REPLACE(@Macro, '<#YEAR>', CONVERT(VARCHAR(4), YEAR(GETDATE())))
	SET @Macro = REPLACE(@Macro, '<#MONTH>', CONVERT(VARCHAR(4), MONTH(GETDATE())))
	SET @Macro = REPLACE(@Macro, '<#DAY>', CONVERT(VARCHAR(4), DAY(GETDATE())))
	SET @Macro = REPLACE(@Macro, '<#DEPT>', @Dept)
	
	IF (@MacroIDGeneration=1) AND (@IDMacro IS NOT NULL)
	BEGIN
	  -- ID Number is unique for the sub macro
		SET @IDMacro = REPLACE(@IDMacro, '<#YEAR>', CONVERT(VARCHAR(4), YEAR(GETDATE())))
		SET @IDMacro = REPLACE(@IDMacro, '<#MONTH>', CONVERT(VARCHAR(4), MONTH(GETDATE())))
		SET @IDMacro = REPLACE(@IDMacro, '<#DAY>', CONVERT(VARCHAR(4), DAY(GETDATE())))
		SET @IDMacro = REPLACE(@IDMacro, '<#DEPT>', @Dept)
	
		IF NOT EXISTS(SELECT 1 FROM Macro_Generated_ID WHERE Number_Type=@NumberType AND Macro_Output=@IDMacro)
		BEGIN
			INSERT INTO Macro_Generated_ID VALUES (@NumberType, @IDMacro, 1)
			SET @LastID=@IDSeed
		END
		ELSE BEGIN
			SELECT @LastID=Last_Macro_ID FROM Macro_Generated_ID WHERE Number_Type=@NumberType AND Macro_Output=@IDMacro
			WHILE 1=1
			BEGIN
				SET @LastID = @LastID + 1
			  UPDATE Macro_Generated_ID
					SET Last_Macro_ID=@LastID
				WHERE Number_Type=@NumberType
					AND Macro_Output=@IDMacro
					AND Last_Macro_ID=@LastID-1
				IF @@ROWCOUNT>0 
					BREAK
			END
		END		
	END
	ELSE BEGIN
	  -- ID Number is globally unique
	
		-- Increment the number in a guaranteed unique way
		WHILE 1=1
		BEGIN
			SET @LastID = @LastID + 1
		  UPDATE Macro
				SET Last_Global_ID=@LastID
			WHERE Number_Type=@NumberType
				AND Last_Global_ID=@LastID-1
			IF @@ROWCOUNT>0 
				BREAK
		END
	
	END
	
	-- Finally, update the IDs
	SET @Macro = REPLACE(@Macro, '<#ID>', CONVERT(VARCHAR(10), @LastID))
END

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Macro_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Macro_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Macro_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Macro_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Macro_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Macro_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Macro_Get TO [Dev - JNCC SQL]
END

GO

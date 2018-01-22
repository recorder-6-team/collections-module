/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetLoanDates]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_GetLoanDates
GO

/*===========================================================================*\
  Description:	Returns the dates of a loan.

  Parameters:	@Key	Movement_Key

  Created:	December 2003

  Last revision information:
    $Revision: 2 $
    $Date: 6/05/04 15:00 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_GetLoanDates]
	(@Key char(16))
RETURNS varchar(50)
AS
BEGIN
	DECLARE	@DateString varchar(50)

	DECLARE @OutboundDate varchar(20)
	DECLARE @InboundDate varchar(20)
	DECLARE @ExpectedDate varchar(20)
	
	DECLARE @InboundVagueDateStart int
	DECLARE @ExpectedVagueDateStart int
	
	-- Outbound (start) date
	SELECT TOP 1	@OutboundDate = dbo.ufn_GetDateFromVagueDate(MM.Vague_Date_Start, MM.Vague_Date_End, MM.Vague_Date_Type)
	FROM		Movement_Direction AS MD
	INNER JOIN	Movement_Of_Material AS MM ON MM.Movement_Direction_Key = MD.Movement_Direction_Key
						AND MD.OutBound = 1
	WHERE		MD.Movement_Key = @Key
	ORDER BY	MM.Vague_Date_Start ASC
	
	-- Inbound (end) date
	SELECT TOP 1	@InboundDate = dbo.ufn_GetDateFromVagueDate(MM.Vague_Date_Start, MM.Vague_Date_End, MM.Vague_Date_Type),
			@InboundVagueDateStart = Vague_Date_Start
	FROM		Movement_Direction AS MD
	INNER JOIN	Movement_Of_Material AS MM ON MM.Movement_Direction_Key = MD.Movement_Direction_Key
						AND MD.OutBound = 0
	WHERE		MD.Movement_Key = @Key
	ORDER BY	MM.Vague_Date_Start DESC
	
	-- Getting expected completion date
	SELECT 		@ExpectedDate = dbo.ufn_GetDateFromVagueDate(Exp_Vague_Date_Start, Exp_Vague_Date_End, Exp_Vague_Date_Type),
			@ExpectedVagueDateStart = Exp_Vague_Date_Start
	FROM		Movement
	WHERE		Movement_Key = @Key
	
	-- Choose what to print. If the Expected Completion date is larger than the latest Inbound date, then use the 
	-- expected completion date. Otherwise, use the last inbound date.
	IF @ExpectedVagueDateStart > @InboundVagueDateStart 
		SET @DateString = ISNULL(@OutboundDate + ' - ' + @ExpectedDate, @OutboundDate)
	ELSE
		SET @DateString = ISNULL(@OutboundDate + ' - ' + @InboundDate, @OutboundDate)
	

	RETURN @DateString
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetLoanDates]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_GetLoanDates'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_GetLoanDates TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_GetLoanDates TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_GetLoanDates TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_GetLoanDates TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_GetLoanDates TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_GetLoanDates TO [Dev - JNCC SQL]
	END
GO

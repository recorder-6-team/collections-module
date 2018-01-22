/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetDatePartFromInteger]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_GetDatePartFromInteger
GO

/*===========================================================================*\
  Description:	Returns the specified date part of a date that is expressed as an integer

  Parameters:	@IntegerDateValue - Date as integer
				@DatePartToReturn - Date part to be returned 'y'|'m'|'d'

  Created:	18 September 2003

  Last revision information:
    $Revision: 4 $
    $Date: 6/05/04 11:01 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_GetDatePartFromInteger]
(
@IntegerDateValue INT,
@DatePartToReturn CHAR(1)
)
RETURNS INT

AS
BEGIN

	DECLARE @Y INT
	DECLARE @M INT
	DECLARE @D INT
	DECLARE @ReturnValue INT

	declare @D1 int
	set @D1=365 							-- days in 1 year
	declare @D4 int
	set @D4 = @D1 * 4 + 1			-- days in 4 years, inc leap year
	declare @D100 int
	set @D100 = @D4 * 25 - 1 	-- days in 100 years (no leap year on 100th year)
	declare @D400 int
	set @D400 = @D100 * 4 + 1	-- days in 400 years - every 400 years you do  get a leap year
	--variables
	declare @T int
	declare @I int
	-- get number of days since 1/1/01 
	set @T = @IntegerDateValue+693593
	set @Y=1
	-- find number of whole 400 year blocks
	set @Y = @T / @D400
	set @T = @T - @Y * @D400
	set @Y = @Y * 400 + 1
	set @I = @T / @D100
	set @D = @T - @I * @D100
	if @I=4 begin
		set @I = @I - 1
		set @D = @D + @D100
	end
	set @Y = @Y + @I * 100
	set @I = @D / @D4
	set @D = @D - @I * @D4
	set @Y = @Y + @I * 4
	set @I = @D / @D1
	set @D = @D - @I * @D1
	if @I = 4 begin
		set @I = @I - 1
		set @D = @D + @D1
	end
	set @Y=@Y + @I

	set @M=1
	while 1=1 begin
		set @I = 
			case @M
				when 1 then 31
				when 2 then 
					case when (@Y % 4 = 0) and ((@Y % 100 <> 0) or (@Y % 400 = 0)) then 29 else 28 end
				when 3 then 31
				when 4 then 30
			  	when 5 then 31
				when 6 then 30
				when 7 then 31
				when 8 then 31
				when 9 then 30
				when 10 then 31
				when 11 then 30
				when 12 then 31
			end
		if @D<@I break
		set @D = @D - @I
		set @M = @M + 1
	end

	IF UPPER(@DatePartToReturn) = 'Y'
		SET @ReturnValue = @Y
	ELSE IF UPPER(@DatePartToReturn) = 'M'
		SET @ReturnValue = @M
	ELSE IF UPPER(@DatePartToReturn) = 'D'
		SET @ReturnValue = @D+1 -- + 1 because 1st day if month is day 1, not zero

	RETURN @ReturnValue
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetDatePartFromInteger]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_GetDatePartFromInteger'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_GetDatePartFromInteger TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_GetDatePartFromInteger TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_GetDatePartFromInteger TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_GetDatePartFromInteger TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_GetDatePartFromInteger TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_GetDatePartFromInteger TO [Dev - JNCC SQL]
	END
GO

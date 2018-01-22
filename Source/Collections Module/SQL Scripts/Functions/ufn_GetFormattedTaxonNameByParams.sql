/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetFormattedTaxonNameByParams]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_GetFormattedTaxonNameByParams
GO

/*===========================================================================*\
  Description:	Returns the Taxon name as a formatted string given the input parameters.
		The rule for Taxon name format is:
			Common_Name + (Actual_Name Authority)
		where only the Actual_Name is mandatory and other fields can be in italics

		Null fields are omitted.

  Parameters:	@Actual_Name 			Actual name of Taxon
				@Actual_Name_Italic		Whether name should be in italics
				@Common_Name			Common name of Taxon
				@Common_Name_Italic		Whether name should be in italics
				@Authority				Taxon Authority
				@ShowCommonNames 		Whether or not Common names should be shown

  Created:	28 August 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/05/04 11:01 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_GetFormattedTaxonNameByParams]
(
@Actual_Name VARCHAR(60),
@Actual_Name_Italic BIT,
@Common_Name VARCHAR(60),
@Common_Name_Italic BIT,
@Authority VARCHAR(65),
@ShowCommonNames BIT

)
RETURNS varchar(185)


AS
BEGIN
	DECLARE	@FormattedName varchar(185)
	SET @FormattedName = ''

	--Ignore common name if not required or same as preferred name
	IF @ShowCommonNames = 0 OR @Actual_Name=@Common_Name
		SET @Common_Name = NULL

	SET @FormattedName = 
	CASE 
	WHEN @Common_Name IS NULL THEN
		CASE @Actual_Name_Italic
		WHEN 0 THEN
			CASE WHEN @Authority IS NULL THEN 
				@Actual_Name
			ELSE 
				@Actual_Name + ' ' + @Authority
			END
		ELSE
			CASE WHEN @Authority IS NULL THEN 
				'<i>' + @Actual_Name + '</i>'
			ELSE 
				'<i>' + @Actual_Name + '</i> ' + @Authority
			END
		END
	WHEN @Common_Name_Italic = 0 THEN
		CASE @Actual_Name_Italic
		WHEN 0 THEN
			CASE WHEN @Authority IS NULL THEN 
				@Common_Name + ' (' + @Actual_Name + ')'
			ELSE 
				@Common_Name + ' (' + @Actual_Name + ' ' + @Authority + ')' 
			END
		ELSE 
			CASE WHEN @Authority IS NULL THEN 
				@Common_Name + ' (<i>' + @Actual_Name + '</i>)'
			ELSE 
				@Common_Name + ' (<i>' + @Actual_Name + '</i> ' + @Authority + ')'
			END
		END
	WHEN @Common_Name_Italic = 1 THEN
		CASE @Actual_Name_Italic
		WHEN 0 THEN
			CASE WHEN @Authority IS NULL THEN 
				'<i>' + @Common_Name + '</i> (' + @Actual_Name + ')'
			ELSE 
				'<i>' + @Common_Name + '</i> (' + @Actual_Name + ' ' + @Authority + ')' 
			END
		ELSE 
			CASE WHEN @Authority IS NULL THEN 
				'<i>' + @Common_Name + '</i> (<i>' + @Actual_Name + '</i>)'
			ELSE 
				'<i>' + @Common_Name + '</i> (<i>' + @Actual_Name + '</i> ' + @Authority + ')'
			END
		END
	END

	RETURN @FormattedName
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetFormattedTaxonNameByParams]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_GetFormattedTaxonNameByParams'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_GetFormattedTaxonNameByParams TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_GetFormattedTaxonNameByParams TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_GetFormattedTaxonNameByParams TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_GetFormattedTaxonNameByParams TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_GetFormattedTaxonNameByParams TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_GetFormattedTaxonNameByParams TO [Dev - JNCC SQL]
	END
GO
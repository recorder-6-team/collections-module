/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetFieldCollectors]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_GetFieldCollectors
GO

/*===========================================================================*\
  Description:	Gets the field collectors for a collection_unit_key. If 
		there is only one field collector for a collection_unit_key, 
		then just return that one surname. If there are two, return
		two surnames. If there are more than two, then return the
		first surname and 'et al.'. The surnames are sorted so that
		the survey_event_recorder with the earliest entry_date is
		the first surname returned.

  Parameters:	@CollectionUnitKey

  Created:	December 2003

  Last revision information:
    $Revision: 2 $
    $Date: 6/05/04 15:00 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_GetFieldCollectors]
	(@CollectionUnitKey char(16))
RETURNS varchar(70)
AS
BEGIN
	DECLARE	@FieldCollectors varchar(70)
	DECLARE @Surname varchar(30)
	DECLARE @RowCount int
	SET @FieldCollectors = ''
	
	-- Firstly, we need to know how many rows the select statement will bring back. 
	-- NB. If there is a way of getting the row count from the select within the cursor, then this
	-- first select statement that only gets the count could be removed. @@RowCount doesn't work within
	-- the cursor, so this was the only way I could think of doing it.
	SELECT		
			@RowCount = Count(*)
	FROM		Specimen_Field_Data AS SFD
	LEFT JOIN	Occurrence AS O ON O.Occurrence_Key = SFD.Occurrence_Key
	LEFT JOIN	Taxon_Occurrence AS XO ON XO.Taxon_Occurrence_Key = SFD.Taxon_Occurrence_Key
	INNER JOIN	Sample_Recorder AS SR ON (SR.Sample_Key = O.Sample_Key OR SR.Sample_Key = XO.Sample_Key)
	INNER JOIN	Survey_Event_Recorder AS SER ON SER.SE_Recorder_Key = SR.SE_Recorder_Key
	INNER JOIN	Individual AS I ON I.Name_Key = SER.Name_Key
	WHERE 		SFD.Collection_Unit_Key = @CollectionUnitKey
	AND 		SFD.Gathering_Event = 1

	-- Declare the cursor and do the select statement to get the surnames
	DECLARE csrFieldCollector CURSOR FAST_FORWARD
	FOR
		SELECT		
				I.Surname AS GathererName
		FROM		Specimen_Field_Data AS SFD
		LEFT JOIN	Occurrence AS O ON O.Occurrence_Key = SFD.Occurrence_Key
		LEFT JOIN	Taxon_Occurrence AS XO ON XO.Taxon_Occurrence_Key = SFD.Taxon_Occurrence_Key
		INNER JOIN	Sample_Recorder AS SR ON (SR.Sample_Key = O.Sample_Key OR SR.Sample_Key = XO.Sample_Key)
		INNER JOIN	Survey_Event_Recorder AS SER ON SER.SE_Recorder_Key = SR.SE_Recorder_Key
		INNER JOIN	Individual AS I ON I.Name_Key = SER.Name_Key
		WHERE 		SFD.Collection_Unit_Key = @CollectionUnitKey
		AND 		SFD.Gathering_Event = 1
		ORDER BY	SER.Entry_Date ASC
	
	-- If there are any field collectors associated with this collection_unit_key...
	IF @RowCount > 0 
	BEGIN
		OPEN csrFieldCollector		

		-- Put the first surname into the string.
		FETCH NEXT FROM csrFieldCollector INTO @Surname
		IF @@FETCH_STATUS = 0 SELECT @FieldCollectors = @Surname
	
		-- If there are only two surnames returned, show the second one.
		IF @RowCount = 2
		BEGIN
			FETCH NEXT FROM csrFieldCollector INTO @Surname
			IF @@FETCH_STATUS = 0 SELECT @FieldCollectors = @FieldCollectors + ' + ' + @Surname
		END

		-- If there are more than two surnames returned, only show the first one, then add '+ et al.' to it.
		IF @RowCount > 2
		BEGIN
			SELECT @FieldCollectors = @FieldCollectors + ' et al.'
		END
	
		CLOSE csrFieldCollector
	END
	
	DEALLOCATE csrFieldCollector

	RETURN @FieldCollectors
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetFieldCollectors]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_GetFieldCollectors'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_GetFieldCollectors TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_GetFieldCollectors TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_GetFieldCollectors TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_GetFieldCollectors TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_GetFieldCollectors TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_GetFieldCollectors TO [Dev - JNCC SQL]
	END
GO

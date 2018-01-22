/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpecimenBestLocationName_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpecimenBestLocationName_Get]
GO

/*===========================================================================*\
  Description:	Returns the most appropriate value for the 'name' of a location.

  Parameters:	@Key	Collection unit key
		@Name	OUTPUT

  Created:	March 2014

  Last revision information:
    $Revision: 2 $
    $Date: 17/03/14 16:58 $
    $Author: Christopherknight $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimenBestLocationName_Get]
	@Key char(16),
	@Name varchar(100) OUTPUT
AS
    DECLARE @Temp varchar(100) 
	IF EXISTS(SELECT * FROM Specimen_Field_Data WHERE Collection_Unit_Key = @Key AND Gathering_Event = 1)
	BEGIN	
		-- A temporary table to hold information about the specific sample.
		CREATE TABLE #temporarySampleTable(
		Sample_Location_Key char(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
		Sample_Survey_Event_Key char(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
		Sample_Location_Name varchar(100) COLLATE SQL_Latin1_General_CP1_CI_AS
		)
		
		INSERT INTO #temporarySampleTable(Sample_Location_Key, Sample_Survey_Event_Key, Sample_Location_Name)
		SELECT		S.Location_Key, S.Survey_Event_Key, S.Location_Name
		FROM		Specimen_Field_Data AS SFD
		LEFT JOIN	Taxon_Occurrence AS XO ON XO.Taxon_Occurrence_Key = SFD.Taxon_Occurrence_Key
		LEFT JOIN	Occurrence AS O ON O.Occurrence_Key = SFD.Occurrence_Key
		INNER JOIN	[Sample] AS S ON (S.Sample_Key = O.Sample_Key OR S.Sample_Key = XO.Sample_Key)
		WHERE		SFD.Collection_Unit_Key = @Key

		---
		SELECT		@Temp = LN.Item_Name
		FROM		#temporarySampleTable AS TST
		INNER JOIN	Location_Name AS LN ON LN.Location_Key = TST.Sample_Location_Key
		WHERE		LN.Preferred = 1

        -- Set @Name as the 'Event Location'.
        IF (@Temp IS NOT NULL)
			SET @Name = @Temp
        ELSE
        BEGIN
			SELECT		Top 1 @Temp = T.Plaintext
			FROM		#temporarySampleTable AS TST	
			INNER JOIN Survey_Event SE ON SE.Survey_Event_Key = TST.Sample_Survey_Event_Key
			INNER JOIN Survey_Event_Geo_Area SEGA ON SEGA.Survey_Event_Key = SE.Survey_Event_Key
			INNER JOIN Concept C ON C.Concept_Key = SEGA.Concept_Key
			INNER JOIN Concept C1 ON C1.Meaning_Key = C.Meaning_Key AND C1.List_Preferred = 1
			INNER JOIN Term T ON T.Term_Key = C1.Term_Key
			ORDER BY
				C1.Sort_Code, T.Plaintext

			-- Set @Name as the 'First Geographic Area'.
			IF (@Temp IS NOT NULL)
				SET @Name = @Temp
			ELSE
			BEGIN
				SELECT		@Temp = TST.Sample_Location_Name
				FROM		#temporarySampleTable AS TST

				-- Set @Name as the 'Location Name'.
				IF (@Temp IS NOT NULL)
					SET @Name = @Temp
				ELSE
				BEGIN
					SELECT		@Temp = SE.Spatial_Ref
					FROM		#temporarySampleTable AS TST	
					INNER JOIN Survey_Event SE ON SE.Survey_Event_Key = TST.Survey_Survey_Event_Key

					-- Set @Name as the 'Spatial Reference'.
					IF (@Temp IS NOT NULL)
						SET @Name = @Temp
					ELSE
						SET @Name = 'Unknown'
				END
			END
        END
	DROP TABLE #temporarySampleTable
    END
	ELSE
		SET @Name = 'Unknown'

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimenBestLocationName_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimenBestLocationName_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimenBestLocationName_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimenBestLocationName_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimenBestLocationName_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenBestLocationName_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenBestLocationName_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimenBestLocationName_Get TO [Dev - JNCC SQL]
END

GO
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
    $Revision: 3 $
    $Date: 20/03/14 10:59 $
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

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_QESessionsFormNames_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_QESessionsFormNames_Select]
GO

/*===========================================================================*\
  Description:	Returns the list of unique form names associated with available
                quick entry sessions.

  Parameters:	@UserDomainMask

  Created:	March 2014

  Last revision information:
    $Revision: 3 $
    $Date: 20/03/14 10:59 $
    $Author: Christopherknight $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QESessionsFormNames_Select]
	@UserDomainMask int

 AS

SET NOCOUNT ON

SELECT DISTINCT
		QS.QE_Template_Key,
		QT.Item_Name
FROM QE_Session QS
		INNER JOIN QE_Template QT ON QT.QE_Template_Key=QS.QE_Template_Key
		INNER JOIN Domain D ON (D.Subject_Area_Key=QT.Subject_Area_Key
				AND D.Domain_Mask & @UserDomainMask > 0) OR 
				QT.Subject_Area_Key IS NULL 
ORDER BY QT.Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QESessionsFormNames_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QESessionsFormNames_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QESessionsFormNames_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QESessionsFormNames_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QESessionsFormNames_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_QESessionsFormNames_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QESessionsFormNames_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QESessionsFormNames_Select TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_QESessionFormNamesCount_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_QESessionFormNamesCount_Get]
GO

/*===========================================================================*\
  Description:	Returns the count of quick entry form types for available 
  sessions.

  Parameters:	@Count	OUTPUT
				@UserDomainMask

  Created:	March 2014

  Last revision information:
    $Revision: 3 $
    $Date: 20/03/14 10:59 $
    $Author: Christopherknight $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QESessionFormNamesCount_Get]
	@Count int OUTPUT,
	@UserDomainMask int

AS

  SELECT @Count=COUNT(DISTINCT QT.QE_Template_Key) 
  FROM QE_Session QS
	INNER JOIN QE_Template QT ON QT.QE_Template_Key=QS.QE_Template_Key
	INNER JOIN Domain D ON (D.Subject_Area_Key=QT.Subject_Area_Key
		AND D.Domain_Mask & @UserDomainMask > 0)
		OR QT.Subject_Area_Key IS NULL
	
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QESessionFormNamesCount_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QESessionFormNamesCount_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        GRANT EXECUTE ON dbo.usp_QESessionFormNamesCount_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QESessionFormNamesCount_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QESessionFormNamesCount_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_QESessionFormNamesCount_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QESessionFormNamesCount_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_QESessionFormNamesCount_Get TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_QESessionsForm_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_QESessionsForm_Select]
GO

/*===========================================================================*\
  Description:	Returns the list of quick entry sessions available 
				(for a given template).

  Parameters:	@QE_Template_Key
				@UserDomainMask

  Created:	March 2014

  Last revision information:
    $Revision: 3 $
    $Date: 20/03/14 10:59 $
    $Author: Christopherknight $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QESessionsForm_Select]
    @QE_Template_Key char(16),
	@UserDomainMask int

 AS

SET NOCOUNT ON

SELECT DISTINCT
		QS.QE_Session_Key,
		QS.Item_Name
FROM QE_Session QS
		INNER JOIN QE_Template QT ON QT.QE_Template_Key=QS.QE_Template_Key
		INNER JOIN Domain D ON (D.Subject_Area_Key=QT.Subject_Area_Key
				AND D.Domain_Mask & @UserDomainMask > 0) OR 
				QT.Subject_Area_Key IS NULL
WHERE QT.QE_Template_Key = @QE_Template_Key
ORDER BY QS.Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QESessionsForm_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QESessionsForm_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        GRANT EXECUTE ON dbo.usp_QESessionsForm_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QESessionsForm_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QESessionsForm_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_QESessionsForm_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QESessionsForm_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_QESessionsForm_Select TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_QESessionFormCount_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_QESessionFormCount_Get]
GO

/*===========================================================================*\
  Description:	Returns the count of quick entry sessions, for a given template,
				available.

  Parameters:	@Count	OUTPUT
				@UserDomainMask

  Created:	March 2014

  Last revision information:
    $Revision: 3 $
    $Date: 20/03/14 10:59 $
    $Author: Christopherknight $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QESessionFormCount_Get]
	@Count int OUTPUT,
    @QE_Template_Key char(16),
	@UserDomainMask int

AS

  SELECT @Count=COUNT(DISTINCT QE_Session_Key) 
  FROM QE_Session QS
	INNER JOIN QE_Template QT ON QT.QE_Template_Key=QS.QE_Template_Key
	INNER JOIN Domain D ON (D.Subject_Area_Key=QT.Subject_Area_Key
		AND D.Domain_Mask & @UserDomainMask > 0)
		OR QT.Subject_Area_Key IS NULL
  WHERE QT.QE_Template_Key = @QE_Template_Key
	
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QESessionFormCount_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QESessionFormCount_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        GRANT EXECUTE ON dbo.usp_QESessionFormCount_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QESessionFormCount_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QESessionFormCount_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_QESessionFormCount_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QESessionFormCount_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_QESessionFormCount_Get TO [Dev - JNCC SQL]
END

GO

IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_QESessionLastUser_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_QESessionLastUser_Select]
GO
    

/*===========================================================================*\
  Description:	Returns the last user of a quick entry session

  Parameters:	
		@QESessionKey - @QE_SEssion_Key

  Created:	March 2014

  Last revision information:
    $Revision: 3 $
    $Date: 20/03/14 10:59 $
    $Author: Christopherknight $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QESessionLastUser_Select]
  @QESessionKey as int
AS

SET NOCOUNT ON
IF EXISTS (
	SELECT QS.Changed_Session_ID
	FROM QE_Session QS
	WHERE QS.QE_Session_Key = @QESessionKey
    AND QS.Changed_Session_ID IS NOT NULL)

    SELECT dbo.ufn_GetFormattedName(S.User_Name_Key) AS [User_Name]
	FROM QE_Session QS
	INNER JOIN Session S 
		ON QS.Changed_Session_ID = S.Session_ID
	WHERE QS.QE_Session_Key = @QESessionKey
ELSE
	SELECT dbo.ufn_GetFormattedName(S.User_Name_Key) AS [User_Name]
	FROM QE_Session QS
	INNER JOIN Session S 
		ON QS.Entered_Session_ID = S.Session_ID
	WHERE QS.QE_Session_Key = @QESessionKey  
		
GO 

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QESessionLastUser_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QESessionLastUser_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        GRANT EXECUTE ON dbo.usp_QESessionLastUser_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QESessionLastUser_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QESessionLastUser_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_QESessionLastUser_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QESessionLastUser_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_QESessionLastUser_Select TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimensLabel_CreateSpecimenLabel') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_SpecimensLabel_CreateSpecimenLabel]
GO

/*===========================================================================*\
  Description:	Creates a new label record for this specimen.

  Parameters:	Collection_Unit_Key,
				SessionID

  Created:	March 2014

  Last revision information:
    $Revision: 3 $
    $Date: 20/03/14 10:59 $
    $Author: Christopherknight $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimensLabel_CreateSpecimenLabel]
(
	@Collection_Unit_Key CHAR(16),
	@SessionID CHAR(16)
)
AS
	DECLARE @Specimen_Label_Key CHAR(16)
	
	DECLARE @Inscription VARCHAR(1024)
	
	SET @Inscription = 'Registration Number: ' +
			dbo.ufn_GetPrefNumber(@Collection_Unit_Key) + CHAR(13) + CHAR(10) +
			'Determinations:' + CHAR(9)
	
	DECLARE @Determinations TABLE
	(
		RowNumber INT IDENTITY(1, 1),
		Domain VARCHAR(100),
		Determination VARCHAR(100)
	)

	INSERT INTO		@Determinations (Domain, Determination)
	SELECT			DOM.Item_Name											AS	Domain,
					CASE
						WHEN SU.Life_Sciences = 0
						THEN ISNULL(CT.Item_Name, 'No Determination') 
						ELSE ISNULL(dbo.ufn_GetFormattedTaxonNameByParams(
								ITN.Actual_Name, ITN.Actual_Name_Italic,
								ITN.Common_Name, ITN.Common_Name_Italic,
								ITN.Authority, 1), 'No Determination') 
					END														AS	Determination 
	FROM			dbo.Determination										AS	D
	INNER JOIN		Specimen_Unit											AS	SU
	ON				D.Specimen_Collection_Unit_Key							=	SU.Collection_Unit_Key
	LEFT JOIN		Taxon_Determination										AS	TD
	ON				SU.Collection_Unit_Key									=	TD.Specimen_Collection_Unit_Key
	LEFT JOIN		Index_Taxon_Name										AS	ITN
	ON				ITN.Taxon_List_Item_Key									=	TD.Taxon_List_Item_Key
	INNER JOIN 		VW_ConceptTerm											AS	CT
	ON				CT.Concept_Key											=	D.Concept_Key
	INNER JOIN		Concept_Group											AS  g
    ON				g.Concept_Group_Key										=   CT.Concept_Group_Key
    INNER JOIN		Local_Domain											AS  l
    ON				l.Local_Domain_Key										=   g.Local_Domain_Key
    INNER JOIN		Domain													AS  DOM
    ON				DOM.Domain_Key											=   l.Domain_Key
	WHERE			D.Specimen_Collection_Unit_Key							=	@Collection_Unit_Key
	ORDER BY		D.Preferred DESC

	DECLARE @DeterminationCount INT
	SET @DeterminationCount = (SELECT COUNT(*) FROM @Determinations)
	DECLARE @i INT
	SET @i = 1
	
	WHILE @i < @DeterminationCount + 1
	BEGIN
		IF @i = 1
		BEGIN
			SET @Inscription = @Inscription + '*' +
					(SELECT Domain FROM @Determinations WHERE RowNumber = @i) + ' - ' +
					(SELECT Determination FROM @Determinations WHERE RowNumber = @i)
		END
		ELSE
			SET @Inscription = @Inscription + CHAR(13) + CHAR(10) + CHAR(9) + CHAR(9) +
					(SELECT Domain FROM @Determinations WHERE RowNumber = @i) + ' - ' +
					(SELECT Determination FROM @Determinations WHERE RowNumber = @i)
		SET @i = @i + 1
	END
	
	DECLARE @Collectors TABLE
	(
		RowNumber INT IDENTITY(1, 1),
		FullName VARCHAR(51)
	)

	INSERT INTO		@Collectors (FullName)
	SELECT			I.Surname + CASE
									WHEN I.Forename IS NULL
									THEN ''
									ELSE ', ' + I.Forename
								END									AS	FullName
	FROM			Specimen_Field_Data								AS	SFD
	LEFT JOIN		Occurrence										AS	O
	ON				O.Occurrence_Key								=	SFD.Occurrence_Key
	LEFT JOIN		Taxon_Occurrence								AS	XO
	ON				XO.Taxon_Occurrence_Key							=	SFD.Taxon_Occurrence_Key
	INNER JOIN		Sample_Recorder									AS	SR
	ON				(SR.Sample_Key									=	O.Sample_Key
	OR				SR.Sample_Key									=	XO.Sample_Key)
	INNER JOIN		Survey_Event_Recorder							AS	SER
	ON				SER.SE_Recorder_Key								=	SR.SE_Recorder_Key
	INNER JOIN		dbo.Individual										AS	I
	ON				I.Name_Key										=	SER.Name_Key
	WHERE 			SFD.Collection_Unit_Key							=	@Collection_Unit_Key
	AND 			SFD.Gathering_Event								=	1
	ORDER BY		I.Surname ASC	

	DECLARE @CollectorCount INT
	SET @CollectorCount = (SELECT COUNT(*) FROM @Collectors)
	SET @i = 1
	
	SET @Inscription = @Inscription + CHAR(13) + CHAR(10) + 'Field Collectors: '

	WHILE @i < @CollectorCount + 1
	BEGIN
		IF @i > 1
			SET @Inscription = @Inscription + '; '

		SET @Inscription = @Inscription + (SELECT FullName FROM @Collectors WHERE RowNumber = @i)
		
		SET @i = @i + 1
	END
	
	SET @Inscription = @Inscription + CHAR(13) + CHAR(10) + 'Gathering Site: ' +
			ISNULL(dbo.ufn_GetSpecimenGatheringSite(@Collection_Unit_Key), '')

	DECLARE @DateGathered VARCHAR(50)
	EXEC usp_SpecimenDate_Get @Collection_Unit_Key, 'dd/MM/yy', @DateGathered OUTPUT	

	SET @Inscription = @Inscription + CHAR(13) + CHAR(10) +
			'Gathering Date: ' + @DateGathered

	BEGIN TRANSACTION	

		EXECUTE spNextKey 'Specimen_Label', @Specimen_Label_Key OUTPUT

		INSERT INTO		dbo.Specimen_Label
						(
							Specimen_Label_Key,
							Collection_Unit_Key,
							Is_Inscription,
							Label_Position,
							Inscription,
							Translated,
							Translated_Language_Key,
							Comments,
							Author_Name_Key,
							Inferred_Author,
							Confidence_Concept_Key,
							Entered_Session_ID,
							Changed_Session_ID,
							Custodian,
							Is_Current
						)
		VALUES			(
							@Specimen_Label_Key,
							@Collection_Unit_Key,
							0,
							NULL,
							@Inscription,
							NULL,
							NULL,
							'Label created via Specimen Label Output',
							NULL,
							0,
							NULL,
							@SessionID,
							NULL,
							NULL,
							1
						)
							
	COMMIT TRANSACTION					
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimensLabel_CreateSpecimenLabel') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimensLabel_CreateSpecimenLabel'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        GRANT EXECUTE ON dbo.usp_SpecimensLabel_CreateSpecimenLabel TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimensLabel_CreateSpecimenLabel TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimensLabel_CreateSpecimenLabel TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpecimensLabel_CreateSpecimenLabel TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimensLabel_CreateSpecimenLabel TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_SpecimensLabel_CreateSpecimenLabel TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimen_Delete') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Specimen_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Specimen table.
		Ensures the Domain masks of the containing itmes are also updated.
		The assumpton is that a specimen being deleted is not a 
		container for any other specimens. They must have been either 
		deleted or moved to another container themselves.

  Parameters:	@SpecimenKey

  Created:	July 2003

  Last revision information:
    $Revision: 3 $
    $Date: 20/03/14 10:59 $
    $Author: Christopherknight $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Specimen_Delete]
	@SpecimenKey char(16),
	@Timestamp timestamp = NULL
AS
	DECLARE @ExistingCollectionKey char(16),
		@ExistingContainerKey char(16),
		@SpecimenMask int,
		@SUTimestamp timestamp

	/*-------------------------------------------------------------*\
	| Initialise variables.						|
	\*-------------------------------------------------------------*/
	SELECT		@ExistingCollectionKey = S.Parent_Collection_Collection_Unit_Key, 
			@ExistingContainerKey = CU.Current_Container_Collection_Unit_Key
	FROM		Specimen_Unit S
	INNER JOIN 	Collection_Unit CU ON CU.Collection_Unit_Key = S.Collection_Unit_Key
	WHERE		S.Collection_Unit_Key = @SpecimenKey

	-- Retrieve the mask of the preferred concept for specimen.
	EXECUTE	usp_Get_Concept_Domain_Mask_From_Specimen @SpecimenKey, @SpecimenMask OUTPUT

	/*-------------------------------------------------------------*\
	| Do the table delete first. Or the containers will still have	|
	| the specimen and its mask!					|
	\*-------------------------------------------------------------*/

	BEGIN TRANSACTION
		/*-------------------------------------------------------------*\
		| Before continuing, we need to save the timestamp, because the |
		| update that follows this, could update the timestamp itself	|
		| causing the final delete to fail.				|
		\*-------------------------------------------------------------*/
		SELECT	@SUTimestamp = [Timestamp]
		FROM	Specimen_Unit
		WHERE	Collection_Unit_Key = @SpecimenKey
		/*---------------------------------------------------------------------------*\
		  The Determination table has a relationship to the Specimen_Unit table and 
		  the Specimen_Unit has a relationship to the Determination table (similarly
		  for the Taxon_Determination table). Hence, the Preferred_Determination and
		  Preferred_Taxon_Determination fields in the Specimen_Unit table must be
		  both made NULL before they can be deleted.
		\*---------------------------------------------------------------------------*/
		UPDATE	Specimen_Unit
		SET	Preferred_Determination_Key = NULL,
			Preferred_Taxon_Determination_Key = NULL
		WHERE	Collection_Unit_Key = @SpecimenKey

		/*---------------------------------------------------------------------------*\
          Delete Movement_Collections linked to Specimen being deleted.
		  Movement data for the specimen still exists - but allows specimen to
		  be deleted.
		\*---------------------------------------------------------------------------*/
		DELETE dbo.Movement_Collection_Unit
		WHERE Collection_Unit_Key = @SpecimenKey

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Delete collection data associated with specimen.
		DELETE dbo.Collection_Unit_Check
		WHERE Collection_Unit_Key = @SpecimenKey
	
		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE dbo.Collection_Unit_Data
		WHERE Collection_Unit_Key = @SpecimenKey
	
		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE dbo.Collection_Unit_Enquiry
		WHERE Collection_Unit_Key = @SpecimenKey
	
		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE dbo.Collection_Unit_Funding
		WHERE Collection_Unit_Key = @SpecimenKey
	
		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE dbo.Collection_Unit_History
		WHERE Collection_Unit_Key = @SpecimenKey
	
		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE dbo.Collection_Unit_Material
		WHERE Collection_Unit_Key = @SpecimenKey
	
		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE dbo.Collection_Unit_Name
		WHERE Collection_Unit_Key = @SpecimenKey
	
		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE dbo.Collection_Unit_Number
		WHERE Collection_Unit_Key = @SpecimenKey
	
		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE dbo.Collection_Unit_Process
		WHERE Collection_Unit_Key = @SpecimenKey
	
		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE dbo.Collection_Unit_Relation
		WHERE From_Collection_Unit_Key = @SpecimenKey 
		OR To_Collection_Unit_Key = @SpecimenKey
	
		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE dbo.Collection_Unit_Task
		WHERE Collection_Unit_Key = @SpecimenKey
	
		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE dbo.Collection_Unit_Valuation
		WHERE Collection_Unit_Key = @SpecimenKey
	
		IF @@Error <> 0 GOTO RollbackAndExit

		/*---------------------------------------------------------------------------*\
		  Specimen_Unit -> Determination -> Occurrence
		\*---------------------------------------------------------------------------*/
		-- Delete Determinations linked ONLY to Specimen being deleted.
		DELETE	Determination
		WHERE	Specimen_Collection_Unit_Key = @SpecimenKey
		AND	Occurrence_Key IS NULL

		-- Clear only reference to Specimen for Determinations also linked to Occurrence
		UPDATE	Determination
		SET	Specimen_Collection_Unit_Key = NULL
		WHERE	Specimen_Collection_Unit_Key = @SpecimenKey
		AND	Occurrence_Key IS NOT NULL

		IF @@Error <> 0 GOTO RollbackAndExit

		/*---------------------------------------------------------------------------*\
		  Specimen_Unit -> Taxon_Determination -> Taxon_Occurrence
		\*---------------------------------------------------------------------------*/
		-- Delete Taxon Determinations linked ONLY to Specimen being deleted.
		DELETE	Taxon_Determination
		WHERE	Specimen_Collection_Unit_Key = @SpecimenKey
		AND	Taxon_Occurrence_Key IS NULL

		-- Clear only reference to Specimen for Taxon Determinations also linked to Taxon Occurrence
		UPDATE 	Taxon_Determination
		SET	Specimen_Collection_Unit_Key = NULL
		WHERE	Specimen_Collection_Unit_Key = @SpecimenKey
		AND	Taxon_Occurrence_Key IS NOT NULL

		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------------*\
		  Other tables.
		\*-------------------------------------------------------------*/
		DELETE	Specimen_Field_Data
		WHERE	Collection_Unit_Key = @SpecimenKey

		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE	Specimen_Label		
		WHERE	Collection_Unit_Key = @SpecimenKey	

		IF @@Error <> 0 GOTO RollbackAndExit	

		DELETE	Specimen_Unit
		WHERE	Collection_Unit_Key = @SpecimenKey
		AND	(@Timestamp = @SUTimestamp OR (@Timestamp IS NULL))

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Specimen_Unit WHERE Collection_Unit_Key = @SpecimenKey)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

		/*-------------------------------------------------------------*\
		 @Timestamp is null if proc called from a store related process. 
		 In that case , the Collection_Unit table will be handled by the 
		 store related process that called the proc, and not here.
		\*-------------------------------------------------------------*/
		IF @Timestamp IS NOT NULL
			-- If specimen is also a store, let the dedicated procedure deal with it
			IF EXISTS(SELECT * FROM Store WHERE Collection_Unit_Key = @SpecimenKey)
				EXECUTE	usp_Store_Delete @SpecimenKey
			ELSE
			-- Otherwise, just delete record from Collection_Unit table.
				DELETE	Collection_Unit
				WHERE	Collection_Unit_Key = @SpecimenKey

		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------------*\
		  Now switch specimen bit OFF from container and collection.
		\*-------------------------------------------------------------*/
		-- Update the container mask
		EXECUTE	usp_CollectionUnit_Update_DomainMask @ExistingContainerKey, @SpecimenMask, 0
		-- Update the collection mask
		EXECUTE	usp_Collection_Update_DomainMask @ExistingCollectionKey, @SpecimenMask, 0

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimen_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimen_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimen_Delete TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimen_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimen_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimen_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimen_Delete TO [Dev - JNCC SQL]
END
GO
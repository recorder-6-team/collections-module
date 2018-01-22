/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimensMailMerge_CreateSpecimenLabel') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_SpecimensMailMerge_CreateSpecimenLabel]
GO

/*===========================================================================*\
  Description:	Creates a new label record for this specimen.

  Parameters:	Collection_Unit_Key,
				SessionID

  Created:	November 2009

  Last revision information:
    $Revision: 4 $
    $Date: 15/01/10 10:28 $
    $Author: Simonwood $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimensMailMerge_CreateSpecimenLabel]
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
							'Label exported via Specimen Mail Merge',
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimensMailMerge_CreateSpecimenLabel') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimensMailMerge_CreateSpecimenLabel'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimensMailMerge_CreateSpecimenLabel TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimensMailMerge_CreateSpecimenLabel TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimensMailMerge_CreateSpecimenLabel TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpecimensMailMerge_CreateSpecimenLabel TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimensMailMerge_CreateSpecimenLabel TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimensMailMerge_CreateSpecimenLabel TO [Dev - JNCC SQL]
END
GO
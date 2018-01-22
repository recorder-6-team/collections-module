/*===========================================================================*\
  Drop trigger before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[tr_ConceptHistory_IsCurrentUpdate]') 
	   AND    Type = 'TR')
    DROP TRIGGER [dbo].[tr_ConceptHistory_IsCurrentUpdate]
GO

/*===========================================================================*\
  Description:	The Concept table has an IsCurrent field that is set 
		depending on the Concept_History records associated with it.
		If a Concept has no associated Concept_History records
		then Is_Current is set to 1. If the Concept is associated
		with a Concept_History record that has no expiry details,
		Is_Current is set to 1. Otherwise, Is_Current is set to 0.

  Type:		AFTER INSERT, UPDATE, DELETE

  Created:	March 2004

  Last revision information:
    $Revision: 1 $
    $Date: 23/03/04 10:43 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE TRIGGER [dbo].[tr_ConceptHistory_IsCurrentUpdate]
ON [dbo].[Concept_History]
AFTER INSERT, UPDATE, DELETE
AS 
	/*--------------------------------------------------------------*\
	  Declare local variables and set @IsCurrent to be 0 as default.
	\*--------------------------------------------------------------*/
	DECLARE @IsCurrent bit,
		@CurrentConceptKey char(16)

	SET @IsCurrent = 0

	/*--------------------------------------------------------------*\
	  Get the Concept_Key for the Concept_History record that has
	  just been updated.
	\*--------------------------------------------------------------*/
	IF UPDATE(Concept_Group_Version_To) OR UPDATE(To_Vague_Date_Type)
		SELECT @CurrentConceptKey = Concept_Key FROM Inserted 

	ELSE IF (SELECT Count(*) FROM Deleted) > 0 
		SELECT @CurrentConceptKey = Concept_Key FROM Deleted 
	
	/*--------------------------------------------------------------*\
	  If there has been a change that requires the IsCurrent field
	  to be altered, then go ahead and make the change.
	\*--------------------------------------------------------------*/	
	IF @CurrentConceptKey IS NOT NULL
	BEGIN
		/*-----------------------------------------------------------------*\
		  If Concept has no associated Concept_History records (i.e. CH 
		  record just deleted).
		\*-----------------------------------------------------------------*/
		SELECT 	@IsCurrent = 	CASE WHEN Count(*) = 0 	THEN 1
								ELSE 0
					END
		FROM 	Concept_History
		WHERE	Concept_Key = @CurrentConceptKey

		/*-----------------------------------------------------------------*\
		  Check to see if Concept is associated with any Concept_History 
		  records that have no expiry date.
		\*-----------------------------------------------------------------*/
		IF @IsCurrent = 0
			SELECT 	@IsCurrent = 	CASE WHEN Count(*) = 0 	THEN 0
									ELSE 1
						END
			FROM 	Concept_History
			WHERE	Concept_Key = @CurrentConceptKey
			AND 	Concept_Group_Version_To IS NULL
			AND 	To_Vague_Date_Type IS NULL

		/*----------------------------------------*\
		  Actually update the Concept record.
		\*----------------------------------------*/
		UPDATE 	Concept
		SET	Is_Current = @IsCurrent
		WHERE	Concept_Key = @CurrentConceptKey
	END
GO
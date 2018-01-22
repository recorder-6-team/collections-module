/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select_Imported]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_Concept_Select_Imported]
GO

/*===========================================================================*\
  Description:	List-preferred concepts from the specified group that have
				Timestamp later than the given value.

				Note that @session_id should *not* be named @SessionID so
				that dmGeneral does not automagically supply a value when
				we don't want it to.

  Parameters:   @concept_group_key		Concept group key
				@timestamp				Timestamp
				@session_id				[optional] If specified, restrict
										records to those inserted in that
										session

  Created:		Jan 2004

  Last revision information:
	$Revision: 4 $
	$Date: 3/08/11 14:27 $
	$Author: Simonlewis $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select_Imported]
	@concept_group_key	CHAR(16),
	@timestamp			TIMESTAMP,
	@session_id			CHAR(16)	=	NULL
AS
	SET NOCOUNT ON

	IF @session_id IS NULL
	BEGIN
		SELECT		c.Concept_Key,
					c.Timestamp,
					c.Published_Term		AS	Item_Name
		FROM		Concept					AS	c
		INNER JOIN	Term					AS	t
		ON			t.Term_Key				=	c.Term_Key
		WHERE		c.Concept_Group_Key		=	@concept_group_key
		AND			c.Timestamp				>	ISNULL(@timestamp, 0)
		AND			c.List_Preferred		=	1
	END
	ELSE
	BEGIN
		SELECT		c.Concept_Key,
					c.Timestamp,
					c.Published_Term		AS	Item_Name
		FROM		Concept					AS	c
		INNER JOIN	Term					AS	t
		ON			t.Term_Key				=	c.Term_Key
		WHERE		c.Concept_Group_Key		=	@concept_group_key
		AND			c.Entered_Session_ID	=	@session_id
		AND			c.Timestamp				>	ISNULL(@timestamp, 0)
		AND			c.List_Preferred		=	1
	END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_Imported') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select_Imported'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select_Imported TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select_Imported TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_Imported TO [Dev - JNCC SQL]
END
GO
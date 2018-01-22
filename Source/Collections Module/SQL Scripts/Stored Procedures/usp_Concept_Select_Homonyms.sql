/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'dbo.usp_Concept_Select_Homonyms')
	   AND    Type = 'P')
	DROP PROCEDURE dbo.usp_Concept_Select_Homonyms
GO

/*===========================================================================*\
  Description:	Returns all homonyms of the specified concept.

  Parameters:   
	@concept_key	Concept key

  Created:	May 2011

  Last revision information:
	$Revision: 2 $
	$Date: 3/08/11 14:23 $
	$Author: Simonlewis $

\*===========================================================================*/
CREATE PROCEDURE dbo.usp_Concept_Select_Homonyms
	@Key		CHAR(16)
AS
	SET NOCOUNT ON

	select 
		h.Concept_Key as Item_Key,
		ct.Item_Name + ' (' + cg.Item_Name + ')' as Item_Name
	from		concept c
	inner join	homonym_pair hp
		on		hp.Meaning_Key_1 = c.Meaning_Key or hp.Meaning_Key_2 = c.Meaning_Key
	inner join	concept h
		on		(hp.Meaning_Key_1 = h.Meaning_Key or hp.Meaning_Key_2 = h.Meaning_Key)
		and		(h.Meaning_Key <> c.Meaning_Key)
	inner join 	VW_ConceptTerm ct
	 	on ct.Concept_Key = h.Concept_Key
	left join	Term_Version tv 	
		on tv.Term_Version_Key = h.Term_Version_Key
	inner join	Concept_Group cg
		on cg.Concept_Group_Key = h.Concept_Group_Key
	where c.concept_key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_Homonyms') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select_Homonyms'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select_Homonyms TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select_Homonyms TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_Homonyms TO [Dev - JNCC SQL]
END
GO
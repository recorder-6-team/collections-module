/*===========================================================================*\
  Description:	Delete the redundant 'Botanical Name Constructs' Concept
		Group and associated Concept_Group_Version records.

  Created:	July 2004

  Last revision information:
    $Revision: 1 $
    $Date: 12/07/04 15:14 $
    $Author: Anthonysimpson $

\*===========================================================================*/

BEGIN TRANSACTION

	DELETE	Concept_Group_Version
	WHERE	Concept_Group_Key = 'SYSTEM0000000001'

	IF @@Error <> 0 GOTO RollbackAndExit

	DELETE	Concept_Group
	WHERE	Concept_Group_Key = 'SYSTEM0000000001'

	IF @@Error <> 0 GOTO RollbackAndExit

COMMIT TRANSACTION
RETURN

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

-- Fix Exchanges List Date ordering clause.
UPDATE 	Report_Block_Order
SET		Order_Clause_SQL		=	'ORDER BY M.Exp_Vague_Date_Start, M.Exp_Vague_Date_End'
WHERE	Report_Block_Order_Key 	= 	'SYSTEM000000005U'

-- Fix Valuation duplicates ORDER BY clause.
UPDATE	Report_Block_Order
SET		Order_Clause_SQL		=	'ORDER BY ValueBy'
WHERE	Report_Block_Order_Key 	= 	'SYSTEM000000006M'

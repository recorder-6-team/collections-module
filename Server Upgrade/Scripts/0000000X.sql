-- Update titles of sections
UPDATE	Report_Block
SET	Title = 'Tasks Identified'
WHERE	Report_Block_Key = 'SYSTEM0000000028'

UPDATE	Report_Block
SET	Title = 'Funding List'
WHERE	Report_Block_Key = 'SYSTEM0000000005'

-- Update the Population_SQL for Fundings on a Job Detail report
UPDATE	Report_Block_In_Section
SET	Population_SQL = 'SELECT dbo.ufn_GetDateFromVagueDate(CJF.Vague_Date_Start, CJF.Vague_Date_End, CJF.Vague_Date_Type) AS [Date], dbo.ufn_GetFormattedName(CJF.Funded_By_Name_Key) AS FundedBy, ISNULL(CAST(TF.Data AS VARCHAR(10)), '''') + CONVERT(VARCHAR(20), Amount, 1) + CASE WHEN TF.Data IS NULL THEN ISNULL('' '' + C.Item_Name, '''') ELSE '''' END AS Amount, CJF.Details FROM Conservation_Job_Funding AS CJF LEFT JOIN vw_ConceptTerm C ON CJF.Currency_Concept_Key = C.Concept_Key LEFT JOIN Thesaurus_Fact TF ON (TF.Meaning_Key = C.Meaning_Key AND TF.Fact_Type_Concept_Key = ''SYSTEM000000009A'') WHERE CJF.Conservation_Job_Key = ''<#ReportKey>'''
WHERE	Report_Section_Key = 'SYSTEM000000000M'


-- Drop redundant reporting stored procs
IF EXISTS(SELECT * FROM SYSOBJECTS WHERE NAME='usp_ReportSectionItemNameMacro_Get')
	DROP PROCEDURE usp_ReportSectionItemNameMacro_Get
IF EXISTS(SELECT * FROM SYSOBJECTS WHERE NAME='usp_ReportBlockOrder_Select')
	DROP PROCEDURE usp_ReportBlockOrder_Select
IF EXISTS(SELECT * FROM SYSOBJECTS WHERE NAME='usp_ReportBlocks_Select')
	DROP PROCEDURE usp_ReportBlocks_Select

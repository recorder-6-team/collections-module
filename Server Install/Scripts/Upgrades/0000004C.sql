SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
	Point 41 from the minutes of the design session held on 2010-11-16
	-- add an index to Movement.Search_Caption.
\*============================================================================*/
CREATE INDEX	IX_Movement_Search_Caption
ON				dbo.Movement (
				Search_Caption)
GO
/*------------------------------------------------------------------------*\
  ufn_GetMovementStatus was incorrectly named - it was actually returning
  a Collection_Unit_Number. The function isn't used anywhere, so it is
  being dropped.
\*------------------------------------------------------------------------*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetMovementStatus]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_GetMovementStatus
GO

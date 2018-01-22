/* Drop Stored Proc that is no longer required */
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_MovementDirectionInboundMaterialKey_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_MovementDirectionInboundMaterialKey_Get]
GO
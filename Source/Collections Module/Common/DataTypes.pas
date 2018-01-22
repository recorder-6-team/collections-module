{===============================================================================
  Unit:        DataTypes

  Defines:     Interfaces
               ----------
               IAdditionalProperties

               Classes
               -------
               TVariantObject

               Other
               -----
               TEditMode               Enumeration
               TEventType              Enumeration
               TMasterFrameType        Enumeration
               TLanguageKeyString      2 characters-long strings
               TSQLServerTimeStamp     Simple type
               TVariantArray           Array type

  Description: Various types, enumerations and interfaces.

  Model:       <none>

  Created:     July 2003

  Last revision information:
    $Revision: 48 $
    $Date: 10/10/12 11:05 $
    $Author: Alexanderpadley $

===============================================================================}

unit DataTypes;

interface

uses
  SysUtils, Variants;

const
  IID_IAddMenuOptions : TGUID = '{B98E30D4-6B34-4839-8F5A-E9507DD10163}';
  IID_IAdditionalProperties : TGUID = '{A797151E-34D7-4B50-A182-17F2B7D8AD04}';

type
  TVariantArray = Array of Variant;

  TEventType = (etNavigation, etAddSpecimen, etNavigateSpecimen,
                etAddDeterminationLifeScience, etAddDeterminationEarthScience,
                etRefreshSpecimenCaption, etSetEditMode, etSetBrowseMode,
                etTermForManyConceptsChanged, etSortOrderChange,
                etDestroyDetailsContainer, etNilCutNode, etRefreshSelectedNode,
                etInitializeButtons, etClearInterfaceReferences,
                etNavigateStore, etRefreshNodeCaption, etTreeCleared,
                etRefreshSynonymNode, etNodeCleared, etListPreferredChanged,
                etRefreshDetailsContainer, etNavigateMovement,
                etNavigateAccession, etNavigateLoan,
                etNavigateConditionCheck, etNavigateJob, etSelectFolder);

  TMenuType = (mtCopy, mtPaste);                 

  {-----------------------------------------------------------------------------
    Event used to notify parents of an event that requires propogation to the main form.
    Parameters supplied in a variant array as they depend on the event type.
  }
  TFrameNotificationEvent = procedure (Sender: TObject; AType: TEventType; const AParams:
      TVariantArray) of object;

  {-----------------------------------------------------------------------------
    List of types for frames used on different PageControl frames, and receiving
    the data from different sources/database tables.
  }
  TMasterFrameType = (mftUnspecified, mftCollectionUnit, mftCollection,
                      mftSpecimen, mftStore, mftJob, mftOccurrence, mftConcept,
                      mftMeaning, mftTermVersion, mftLocationFeature);

  TNodeContext = (ncCollection, ncSpecimen, ncStore, ncEnquiry, ncValuation,
                  ncAccession, ncLoan, ncMovement, ncConditionCheck, ncJob,
                  ncTask, ncInscriptionLabel, ncRecorder, ncMovementIn,
                  ncMovementOut, ncStoragePlace, ncNone, ncStoreHierarchy);

  TAppliesTo = (atConcept, atMeaning, atTermVersion);

  TSQLSvrTimestamp = OLEVariant;

  TLanguageKeyString = String[2];

  {-----------------------------------------------------------------------------
    Class for wrapping around variants so they can be put in string lists
  }
  TVariantObject = class(TObject)
  private
    FOLEVariant: OLEVariant;
  public
    Constructor Create(AOLEVariant: OLEVariant);
    property OLEVariant: OLEVariant read FOLEVariant write FOLEVariant;
  end;

 {-----------------------------------------------------------------------------
    Interface allowing a flexible number of properties to be read from one class to
    another.
  }
  IAdditionalProperties = interface (IInterface)
    ['{A797151E-34D7-4B50-A182-17F2B7D8AD04}']
    function GetProperty(const AName: String): Variant;
  end;

//==============================================================================
implementation

//==============================================================================
{ TVariantObject }
constructor TVariantObject.Create(AOLEVariant: OLEVariant);
begin
  FOLEVariant := AOLEVariant;
end;

//==============================================================================
end.

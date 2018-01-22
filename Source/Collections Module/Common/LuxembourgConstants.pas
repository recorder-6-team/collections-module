{===============================================================================
  Unit:        LuxembourgConstants

  Defines:     <nothing>

  Description: Contains global constants used in Luxembourg NHM addin for
               Recorder.

  Created:     August 2003

  Last revision information:
    $Revision: 110 $
    $Date: 13/03/14 16:39 $
    $Author: Christopherknight $

===============================================================================}

unit LuxembourgConstants;

interface

uses
  Messages;

const
  //----------------------------------------------------------------------------
  // User-defined messages.
  UM_LOADED = WM_APP + 200;  // 200 is an arbitry value. No special reason for it.

  //----------------------------------------------------------------------------
  // Vague date field names, without prefixes.
  VAGUE_DATE_START = 'Vague_Date_Start';
  VAGUE_DATE_END   = 'Vague_Date_End';
  VAGUE_DATE_TYPE  = 'Vague_Date_Type';

  //----------------------------------------------------------------------------
  // Registry constants.
  JNCC_REG_ROOT_PATH                    = 'Software\Dorset Software\Recorder 6';
  USER_SETTINGS_REG_PATH                = JNCC_REG_ROOT_PATH + '\Settings';
  RECORDER_MACHINE_SETTINGS_REG_PATH    = JNCC_REG_ROOT_PATH;

  COLLECTIONS_MODULE_REG_ROOT_PATH      = 'Software\Dorset Software\Collections Module';
  COLLECTIONS_MACHINE_SETTINGS_REG_PATH = COLLECTIONS_MODULE_REG_ROOT_PATH;
  COLLECTIONS_BROWSER_REG_PATH          = COLLECTIONS_MODULE_REG_ROOT_PATH;
  VIEW_MANAGER_SETTINGS_REG_PATH        = COLLECTIONS_MODULE_REG_ROOT_PATH + '\Settings\ViewManager';
  LIST_HISTORY_REGISTRY_KEY             = COLLECTIONS_MODULE_REG_ROOT_PATH + '\ListHistory';

  THESAURUS_EDITOR_REG_ROOT_PATH        = 'Software\Dorset Software\Thesaurus Editor';
  DELETION_OPTIONS_REG_PATH             = THESAURUS_EDITOR_REG_ROOT_PATH + '\Settings\DeletionOptions';

  // Registry key names
  OPT_DICT_IMAGES_PATH                = 'Dict Images Path';
  OPT_DISPLAY_COMMON_NAMES            = 'Display Common Names';
  OPT_DOMAIN_MASK                     = 'Domain Mask';
  OPT_DRAG_DEST_COLOUR                = 'Drag Destination';
  OPT_DRAG_SOURCE_COLOUR              = 'Drag Source';
  OPT_INCLUDE_HIERARCHY_SYNONYMS      = 'Include Hierarchy Synonyms';
  OPT_LAST_CONCEPT_GROUP              = 'Last Thesaurus Concept Group';
  OPT_LOCAL_IMAGES_PATH               = 'Local Images File Path';
  OPT_LOG_DELETIONS                   = 'Log Deletions As Script';
  OPT_INSTALLATION_PATH               = 'Local Install Dir';
  OPT_MANDATORY_COLOUR                = 'Mandatory';
  OPT_QE_IMPORT_TYPE_INDEX            = 'Quick Entry Import Type';
  OPT_QE_SHOW_AS_FORM                 = 'Quick Entry Form View';
  OPT_SCRIPT_LOG_DELETIONS            = 'File Location Log Deletions Script';
  OPT_SPECFIND_SORT_INDEX             = 'Specimen Finder Sort Order';
  OPT_STD_RPT_TEMPLATE_PATH           = 'Standard Report Template Path';
  OPT_SYNC_TAX_DICT_DELETION          = 'Synchronise Taxon Dictionary Deletion';
  OPT_VIEW_TYPE                       = 'Collections Browser View Type';
  OPT_SPECIMEN_IMAGE_PATH             = 'Specimen Image Path';
  OPT_ADDINS_PATH                     = 'Addin Path';
  OPT_PREFERRED_SYNONYMS_ONLY         = 'Preferred Synonyms Only';
  OPT_GROUP_DETERMINATIONS_BY_DOMAIN  = 'Group Determinations by Domain';
  OPT_USE_ORIGINAL_SPECIMEN_NAMES     = 'Use Original Specimen Names';
  OPT_QUICK_ENTRY_DETERMINATION_LISTS = 'Show Determination Lists in Quick Entry';
  OPT_SHOW_RECORDER_SPECIMENS_TAB     = 'Show Recorder Specimens Tab';

  OPT_RECORDER_INSTALL_PATH      = 'Installation Path';

  //----------------------------------------------------------------------------
  // Domain keys known to and expected by the application.
  DOM_STRATIGRAPHY = 'DSS0039400000001';
  
  //----------------------------------------------------------------------------
  // Local Domain keys known and expected by the application.
  LD_GLOBAL_SYSTEM_TERM_LIST = 'SYSTEM0000000000';

  //----------------------------------------------------------------------------
  // Concept Group keys known and expected by the application.
  CG_ACQUISITION_METHODS                              = 'SYSTEM0000000000';
  CG_BOTANICAL_NAME_CONSTRUCTS                        = 'SYSTEM0000000001';
  CG_COLLECTION_UNIT_NUMBER_TYPES                     = 'SYSTEM0000000002';
  CG_COLLECTION_RISKS                                 = 'SYSTEM0000000003';
  CG_COLLECTION_UNIT_TO_NAME_RELATIONSHIPS            = 'SYSTEM0000000004';
  CG_COMMUNICATION_TYPES                              = 'SYSTEM0000000005';
  CG_CONDITION_CHECK_TYPES                            = 'SYSTEM0000000006';
  CG_CONDITION_CHECK_CONDITIONS                       = 'SYSTEM0000000007';
  CG_CONSERVATION_TASK_TYPES                          = 'SYSTEM0000000008';
  CG_ENQUIRY_METHODS                                  = 'SYSTEM0000000009';
  CG_ENQUIRY_TYPES                                    = 'SYSTEM000000000A';
  CG_LEVELS_OF_CONFIDENCE                             = 'SYSTEM000000000B';
  CG_MEASUREMENT_METHODS                              = 'SYSTEM000000000C';
  CG_MEASUREMENT_UNIT                                 = 'SYSTEM000000000D';
  CG_MEASUREMENT_PARAMETERS                           = 'SYSTEM000000000E';
  CG_MATERIALS                                        = 'SYSTEM000000000F';
  CG_NOMENCLATURAL_STATUSES                           = 'SYSTEM000000000G';
  CG_QUANTITY_UNITS                                   = 'SYSTEM000000000H';
  CG_SPECIMEN_TYPE                                    = 'SYSTEM000000000J';
  CG_STORE_TYPE                                       = 'SYSTEM000000000K';
  CG_THESAURUS_FACT_TYPES                             = 'SYSTEM000000000L';
  CG_THESAURUS_NAME_TYPES                             = 'SYSTEM000000000M';
  CG_TIME_UNITS                                       = 'SYSTEM000000000N';
  CG_VALUATION_TYPES                                  = 'SYSTEM000000000O';
  CG_SPECIMEN_PROCESSES                               = 'SYSTEM000000000P';
  CG_CURRENCIES                                       = 'SYSTEM000000000Q';
  CG_SEASONS                                          = 'SYSTEM000000000R';
  CG_MONTHS                                           = 'SYSTEM000000000S';
  CG_CONCEPT_DESIGNATION_TYPES                        = 'SYSTEM000000000T';
  CG_COLLECTION_PROCESSES                             = 'SYSTEM000000000U';
  CG_STORE_PROCESSES                                  = 'SYSTEM000000000V';
  CG_DESCRIPTOR_PARAMETERS                            = 'SYSTEM000000000W';
  CG_GEO_AREAS                                        = 'SYSTEM000000000X';

  //----------------------------------------------------------------------------
  // Subject Area keys known and expected by the application.
  CG_GEO_SUBJECT                                      = 'DSS0039400000005';

  //----------------------------------------------------------------------------
  //Quick entry field keys
  QE_FIELD_ACCESSION_NUMBER               = 'SYSTEM000000000P';
  QE_FIELD_ACQUISITION_REFERENCE_NUMBER   = 'SYSTEM000000000I';
  QE_FIELD_COLLECTION                     = 'SYSTEM000000000E';
  QE_FIELD_CONFIDENTIAL                   = 'SYSTEM000000000M';
  QE_FIELD_CURRENT_SPECIMEN_LOCATION_CODE = 'SYSTEM000000000L';
  QE_FIELD_CURRENT_SPECIMEN_STORE         = 'SYSTEM000000000J';
  QE_FIELD_DEPARTMENT                     = 'SYSTEM000000000H';
  QE_FIELD_DETERMINATION                  = 'SYSTEM0000000000';
  QE_FIELD_DETERMINATION_DATE             = 'SYSTEM0000000009';
  QE_FIELD_DETERMINER                     = 'SYSTEM0000000008';
  QE_FIELD_FIELD_COLLECTOR                = 'SYSTEM000000000C';
  QE_FIELD_FIELD_RECORD_TYPE              = 'SYSTEM000000000D';
  QE_FIELD_GATHERING_DATE                 = 'SYSTEM000000000B';
  QE_FIELD_GATHERING_LOCATION_NAME        = 'SYSTEM0000000007';
  QE_FIELD_GATHERING_METHOD               = 'SYSTEM0000000005';
  QE_FIELD_GATHERING_OCCURRENCE_COMMENT   = 'SYSTEM0000000006';
  QE_FIELD_GATHERING_SITE                 = 'SYSTEM0000000003';
  QE_FIELD_GATHERING_SPATIAL_REFERENCE    = 'SYSTEM000000000A';
  QE_FIELD_INSCRIPTION                    = 'SYSTEM000000000G';
  QE_FIELD_LABEL                          = 'SYSTEM000000000F';
  QE_FIELD_REGISTRATION_NUMBER            = 'SYSTEM000000000O';
  QE_FIELD_SPECIMEN_TYPE                  = 'SYSTEM0000000004';
  QE_FIELD_SURVEY                         = 'SYSTEM0000000001';
  QE_FIELD_TAXON_DETERMINATION            = 'SYSTEM0000000002';
  QE_FIELD_USUAL_SPECIMEN_LOCATION_CODE   = 'SYSTEM000000000K';
  QE_FIELD_USUAL_SPECIMEN_STORE           = 'SYSTEM000000000Q';
              
  //----------------------------------------------------------------------------
  // Control type constants
  CT_FREETEXT =         0;
  CT_VAGUE_DATE =       1;
  CT_SPATIAL_REF =      2;
  CT_TAXON =            3;
  CT_CONCEPT =          4;
  CT_CONCEPT_IN_GROUP = 5;
  CT_COLLECTION =       6;
  CT_DEPARTMENT =       7;
  CT_LOCATION =         8;
  CT_SURVEY =           9;
  CT_INDIVIDUAL =       10;
  CT_ORGANISATION =     11;
  CT_SAMPLE_TYPE =      12;
  CT_STORE =            13;
  CT_YES_NO =           14;
  CT_RECORD_TYPE =      15;
  CT_REGISTRATION =     16;
  CT_ACCESSION =        17;
  CT_DETERMINER_ROLE =  18;
  CT_STAFF_RESPONSIBLE =19;
  CT_INFERRED =         20;
  CT_GEO_AREA =         21;

  //----------------------------------------------------------------------------
  //Defaults for entering records into Recorder Tables, or checking data.
  DEFAULT_DETERMINATION_TYPE = 'NBNSYS0000000004';
  DEFAULT_DETERMINER_ROLE    = 'NBNSYS0000000003';
  INDIVIDUAL_UNKNOWN         = 'NBNSYS0000000004';

  REGISTRATION_NUMBER        = 'SYSTEM0000000001';

  SURVEYOR_RECORDER_ROLE              = 'NBNSYS0000000001';

  FIELD_COLLECTOR_RECORD_TYPE         = 'NBNSYS0000000026';
  FIELD_COLLECTOR_RECORD_TYPE_CONCEPT = 'SYSTEM000000220G';

  //----------------------------------------------------------------------------
  //Table name constants
  TN_COLLECTION               = 'Collection';
  TN_COLLECTION_UNIT          = 'Collection_Unit';
  TN_COLLECTION_UNIT_DATA     = 'Collection_Unit_Data';  // Descriptors and Measurements
  TN_COLLECTION_UNIT_HISTORY  = 'Collection_Unit_History';
  TN_COLLECTION_UNIT_MATERIAL = 'Collection_Unit_Material';
  TN_COLLECTION_UNIT_NAME     = 'Collection_Unit_Name';
  TN_COLLECTION_UNIT_NUMBER   = 'Collection_Unit_Number';
  TN_COLLECTION_UNIT_PROCESS  = 'Collection_Unit_Process';
  TN_COLLECTION_UNIT_RELATION = 'Collection_Unit_Relation';
  TN_COLLECTION_UNIT_TASK     = 'Collection_Unit_Task';
  TN_CONCEPT                  = 'Concept';
  TN_CONCEPT_DESIGNATION      = 'Concept_Designation';
  TN_CONCEPT_GROUP            = 'Concept_Group';
  TN_CONCEPT_RANK             = 'Concept_Rank';
  TN_CONCEPT_HISTORY          = 'Concept_History';
  TN_CONCEPT_RELATION         = 'Concept_Relation';
  TN_CONCEPT_GROUP_VERSION    = 'Concept_Group_Version';
  TN_CONSERVATION_CHECK       = 'Conservation_Check';
  TN_CONSERVATION_JOB         = 'Conservation_Job';
  TN_CONSERVATION_JOB_FUNDING = 'Conservation_Job_Funding';
  TN_CONSERVATION_JOB_MATERIAL= 'Conservation_Job_Material';
  TN_CONSERVATION_JOB_STAFF   = 'Conservation_Job_Staff';
  TN_CONSERVATION_TASK        = 'Conservation_Task';
  TN_DETERMINATION            = 'Determination';
  TN_DOMAIN                   = 'Domain';
  TN_DOMAIN_HYPERLINK         = 'Domain_Hyperlink';
  TN_ENQUIRY                  = 'Enquiry';
  TN_LOCATION_FEATURE_DATA    = 'Location_Feature_Data';
  TN_INDIVIDUAL               = 'Individual';
  TN_LOCAL_DOMAIN             = 'Local Domain';
  TN_LOCATION                 = 'Location';
  TN_MEANING_RELATION         = 'Meaning_Relation';
  TN_MIXED_DATA               = 'Mixed Data';
  TN_MOVEMENT                 = 'Movement';
  TN_MOVEMENT_FUNDING         = 'Movement_Funding';
  TN_MOVEMENT_OF_MATERIAL     = 'Movement_Of_Material';
  TN_MOVEMENT_OF_MATERIAL_EXCLUSION = 'Movement_Of_Material_Exclusion';
  TN_MOVEMENT_OF_OWNERSHIP    = 'Movement_Of_Ownership';
  TN_NAME                     = 'Name';
  TN_OCCURRENCE               = 'Occurrence';
  TN_OCCURRENCE_DATA          = 'Occurrence_Data';  // Descriptors and Measurements
  TN_OCCURRENCE_RELATION      = 'Occurrence_Relation';
  TN_ORGANISATION             = 'Organisation';
  TN_ORGANISATION_DEPARTMENT  = 'Organisation_Department';
  TN_REFERENCE                = 'Reference';
  TN_SAMPLE                   = 'Sample';
  TN_SEARCH_TERM              = 'Search_Term';
  TN_SEMANTIC_RELATION        = 'Semantic_Relation';
  TN_SOURCE_JOIN              = 'Source_Join';
  TN_SOURCE_FILE              = 'Source_File';
  TN_SPATIAL_REF              = 'Spatial_Ref';  
  TN_SPECIMEN_FIELD_DATA      = 'Specimen_Field_Data';
  TN_SPECIMEN_LABEL           = 'Specimen_Label';
  TN_SPECIMEN_UNIT            = 'Specimen_Unit';
  TN_STORE                    = 'Store';
  TN_SUBJECT_AREA             = 'Subject_Area';
  TN_SURVEY                   = 'Survey';
  TN_SURVEY_EVENT_RECORDER    = 'Survey_Event_Recorder';
  TN_SURVEY_EVENT_GEO_AREA    = 'Survey_Event_Geo_Area';
  TN_TASK_IDENTIFIED          = 'Task_Identified';
  TN_TAXON                    = 'Taxon';
  TN_TAXON_DETERMINATION      = 'Taxon_Determination';
  TN_TAXON_OCCURRENCE         = 'Taxon_Occurrence';
  TN_TAXON_LIST_ITEM          = 'Taxon_List_Item';
  TN_TERM                     = 'Term';
  TN_TERM_VERSION             = 'Term_Version';
  TN_TERM_VERSION_RELATION    = 'Term_Version_Relation';
  TN_THESAURUS_FACT           = 'Thesaurus_Fact';
  TN_THESAURUS_RELATION_TYPE  = 'Thesaurus_Relation_Type';
  TN_VALUATION                = 'Valuation';

  //----------------------------------------------------------------------------
  //Interface constants
  IID_ADDITIONAL_PROPERTIES : TGUID = '{A797151E-34D7-4B50-A182-17F2B7D8AD04}';

  //----------------------------------------------------------------------------
  //Property constants for IAdditionalProperty usage
  PROP_CONCEPT_GROUP_KEY          = 'ConceptGroupKey';
  PROP_CONCEPT_GROUP_VERSION_KEY  = 'ConceptGroupVersionKey';
  PROP_CONCEPT_KEY                = 'ConceptKey';
  PROP_DISCARD_ON_CANCEL          = 'DiscardOnCancel';
  PROP_DRAG_DROP_KEY              = 'DragDropKey';
  PROP_IS_RELATED_NAME_LEAF_NODE  = 'RelatedNameLeafNode';
  PROP_KEY                        = 'Key';
  PROP_MEANING_KEY                = 'MeaningKey';
  PROP_MOVEMENT_TYPE              = 'MovementType';
  PROP_MOVEMENT_OUTBOUND          = 'MovementOutbound';
  PROP_NODE_CAPTION               = 'NodeCaption';
  PROP_NODE_CLASS                 = 'NodeClass';
  PROP_NODE_CONTEXT               = 'NodeContext';
  PROP_PARENT_KEY                 = 'ParentKey';
  PROP_PARENT_NODE_CAPTION        = 'ParentNodeCaption';
  PROP_PARENT_NODE_CONTEXT        = 'ParentNodeContext';
  PROP_RELATIONSHIP_APPLIES_TO    = 'RelationshipAppliesTo';
  PROP_SPECIMEN_IS_LIFESCIENCES   = 'LifeSciences';
  PROP_TABLE_NAME                 = 'TableName';
  PROP_TERM_VERSION_KEY           = 'TermVersionKey';
  PROP_TOP_NODE_CAPTION           = 'TopNodeCaption';
  PROP_TOP_NODE_CONTEXT           = 'TopNodeContext';
  PROP_WITH_ACQUISITION           = 'WithAcquisition';

  //----------------------------------------------------------------------------
  // Names of domain concept groups used in addition to system concept groups.
  ST_DESCRIPTOR_PARAMETERS  = 'Descriptor Parameters';
  ST_MEASUREMENT_PARAMETERS = 'Measurement Parameters';

  //----------------------------------------------------------------------------
  // Names of persisted lists of concepts stored in the registry
  // for last 10 used combo boxes
  // The '\' is used so that different persistent lists can be stored in the
  // registry for different domain concept groups. The Domain Concept Group key
  // will be appended to the string after the '\'
  PL_DESCRIPTOR_PARAMETERS  = ST_DESCRIPTOR_PARAMETERS + '\';
  PL_MEASUREMENT_PARAMETERS = ST_MEASUREMENT_PARAMETERS + '\';
  PL_SPECIMEN_PROCESSES     = 'Specimen Processes';
  PL_COLLECTION_PROCESSES   = 'Collection Processes';
  PL_STORE_PROCESSES        = 'Store Processes';

  //----------------------------------------------------------------------------
  // Dictionary HTML tags
  HTML_IMAGES       = '#IMAGES';
  HTML_LOCAL_IMAGES = '#LOCAL_IMAGES';

  //----------------------------------------------------------------------------
  // Macro tags.
  MACRO_ID    = '<#ID>';
  MACRO_YEAR  = '<#Year>';
  MACRO_MONTH = '<#Month>';
  MACRO_DAY   = '<#Day>';
  MACRO_DEPT  = '<#Dept>';

  //----------------------------------------------------------------------------
  // Meaning keys.
  MEAN_JPEG   = 'SYSTEM00000000C9';
  MEAN_GIF    = 'SYSTEM00000000CA';
  MEAN_Bitmap = 'SYSTEM00000000CB';

  //----------------------------------------------------------------------------
  // Other

  // Suffix used for thumbnail images in the collections browser
  THUMBNAIL_SUFFIX = '_thumbnail.jpg';

  // Maximum of last-used items persisted to registry
  MAX_LAST_USED = 10;
  SZ_KEY_LENGTH = 17; // Length of Null-terminated string holding a NBN key (16+1)

  // ReportSource values for CollectionsReportMenu/Item
  ITEM_REPORT      = 0;
  TOP_LEVEL_REPORT = 1;
  FOLDER_REPORT    = 2;
  TOP_LEVEL_SPECIMEN_MAIL_MERGE_OUTPUT = 3;
  FOLDER_SPECIMEN_MAIL_MERGE_OUTPUT    = 4;
  TOP_LEVEL_SPECIMEN_LABEL_OUTPUT      = 5;
  FOLDER_SPECIMEN_LABEL_OUTPUT         = 6;
  
  //----------------------------------------------------------------------------
  // ViewTypeManager
  ON_CREATE_SELECT_SORT_INDEX = 1000;

  //----------------------------------------------------------------------------
  // SQL Required to pass temporary filter tables to the PopulateTopLevel procs
  SQL_CREATETEMPFILTER = 'IF object_id(''tempdb..#TempFilter'') IS NULL ' +
      'CREATE TABLE #TempFilter (ItemKey CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS)';

  SQL_INSERTTEMPFILTER = 'INSERT INTO #TempFilter Values (''%s'')';

  SQL_DROPTEMPFILTER = 'IF object_id(''tempdb..#TempFilter'') IS NOT NULL ' +
      'DROP TABLE #TempFilter';

  //----------------------------------------------------------------------------
  // SQL Required to create a temporary table of Field Collectors from the
  // Collections Browser that can be viewed by a separate stored proc.
  SQL_CREATE_TEMP_FIELD_COLLECTORS = 'IF object_id(''tempdb..#TempFieldCollectors'') ' +
      'IS NULL CREATE TABLE #TempFieldCollectors (Name_Key CHAR(16) COLLATE ' +
      'SQL_Latin1_General_CP1_CI_AS)';

  SQL_INSERT_TEMP_FIELD_COLLECTORS = 'INSERT INTO #TempFieldCollectors Values (''%s'')';

  SQL_DROP_TEMP_FIELD_COLLECTORS = 'IF object_id(''tempdb..#TempFieldCollectors'') ' +
      'IS NOT NULL DROP TABLE #TempFieldCollectors';             

  // File names
  FN_IMAGEMAGICK_CONFIG = 'ImageMagickConfig.xml';
  FN_TEMP_FILE_PREFIX = 'CM temp file - ';

  // Error Message if ActiveX server is not installed
  ERR_ACTIVEX_SERVER_NOT_REGISTERED = 'Invalid class string';

//==============================================================================
implementation

end.


/* This is the HideShares plugin for ffss server */
/*       (c) Christophe Calmejane - 2002         */
/*        aka Ze KiLleR / SkyTech                */
/*                                               */
/* http://zekiller.skytech.org                   */
/* mailto : zekiller@skytech.org                 */

/* TODO :
*/

#define HS_NAME      "Hide Shares Plugin"
#define HS_VERSION   "0.1"
#define HS_COPYRIGHT "(c) Ze KiLleR - 2002"
#define HS_DESCRIPTION "Automatically hides all shares with $ as first letter."
#define HS_FILE_PREFIX "FS_Log"

/* The only file we need to include is server.h */
#include "../../src/plugin.h"
#undef malloc
#undef strdup

/* We have to declare a FS_PPlugin structure for our callbacks */
FS_PPlugin Pl;
FSP_TInfos HS_Infos;

void * (*PluginQueryFunc)(int Type,...);

bool OnCheckShowShare(FS_PShare Share)
{
  if((Share->Path != NULL) && (Share->Path[0] != '$'))
    return true;
  return false;
}

/* This is the Init fonction (Name it CAREFULLY) called on each LoadPlugin call */
FS_PLUGIN_EXPORT FS_PPlugin Plugin_Init(void *Info,void *(*QueryFunc)(int Type,...))
{
  /* Get pointer to plugin query function */
  PluginQueryFunc = QueryFunc;

  /* Setting all callbacks to NULL */
  Pl = (FS_PPlugin) malloc(sizeof(FS_TPlugin));
  if(Pl == NULL)
    return NULL;
  memset(Pl,0,sizeof(FS_TPlugin));

  /* Setting plugin infos */
  Pl->size = sizeof(FS_TPlugin);
  Pl->Name = HS_NAME;
  Pl->Copyright = HS_COPYRIGHT;
  Pl->Version = HS_VERSION;

  /* Setting our callbacks */
  Pl->OnCheckShowShare = OnCheckShowShare;

  /* And finaly returning the FS_PPlugin structure to the server.
   * If something goes wrong during this init function, free everything you have allocated and return NULL.
   * UnInit function will not be called in this case.
  */
  return Pl;
}

/* This is the UnInit fonction (Name it CAREFULLY) called on each UnLoadPlugin call */
FS_PLUGIN_EXPORT void Plugin_UnInit(void)
{
}

FS_PLUGIN_EXPORT FSP_PInfos Plugin_QueryInfos(void)
{
  HS_Infos.Name = HS_NAME;
  HS_Infos.Version = HS_VERSION;
  HS_Infos.Copyright = HS_COPYRIGHT;
  HS_Infos.Description = HS_DESCRIPTION;
  return &HS_Infos;
}

/* This is the HideShares plugin for ffss server */
/*       (c) Christophe Calmejane - 2002         */
/*        aka Ze KiLleR / SkyTech                */
/*                                               */
/* http://zekiller.skytech.org                   */
/* mailto : zekiller@skytech.org                 */
/*
    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

/* TODO :
*/

#define HS_NAME      "Hide Shares"
#define HS_VERSION   "1.0"
#define HS_COPYRIGHT "(c) Ze KiLleR - 2002"

/* The only file we need to include is server.h */
#include "../../src/plugin.h"
#undef malloc
#undef strdup

#include "hideshare.h"

char *HS_Lang[HS_LANG_COUNT][HS_LANGS_COUNT] = {/* English */
                                                {"En",
                                                 "Automatically hides shares beginning with $."
                                                },
                                                /* French */
                                                {"Fr",
                                                 "Cache automatiquement les partages dont le nom commence par $."
                                                }
                                               };
#define HS_LANG(x) HS_Lang[HS_CurrentLang][x]

/* We have to declare a FS_PPlugin structure for our callbacks */
FS_PPlugin Pl;
FSP_TInfos HS_Infos;
unsigned int HS_CurrentLang = HS_LANG_ENGLISH;

void * (*PluginQueryFunc)(int Type,...);

void HS_LoadLanguage(void)
{
  char buf[100];
  int i;

  SU_RB_GetStrValue(FFSS_LM_REGISTRY_PATH "FavoriteLanguage",buf,sizeof(buf),"En");
  for(i=0;i<HS_LANG_COUNT;i++)
  {
    if(stricmp(buf,HS_Lang[i][HS_LANGS_COUNTRYCODE]) == 0)
    {
      HS_CurrentLang = i;
      break;
    }
  }
}

bool OnCheckShowShare(FS_PShare Share)
{
  if((Share->ShareName != NULL) && (Share->ShareName[0] != '$'))
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

  HS_LoadLanguage();

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
  HS_LoadLanguage();

  HS_Infos.Name = HS_NAME;
  HS_Infos.Version = HS_VERSION;
  HS_Infos.Copyright = HS_COPYRIGHT;
  HS_Infos.Description = HS_LANG(HS_LANGS_DESCRIPTION);
  return &HS_Infos;
}

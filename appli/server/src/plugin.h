#ifndef __PLUGIN_H__
#define __PLUGIN_H__

/* Undefine c++ bool type (unsigned char ?)
   Use SU_BOOL type in your appli, every time you use a ffss prototype
 */
#ifdef __cplusplus
#define bool SU_BOOL
#endif /* __cplusplus */

#include "server.h"

#define FSP_BASE_REG_KEY "HKEY_CURRENT_USER\\Software\\FFSS\\Server\\PluginsConf\\"

#define FSPQ_ACQUIRE_GLOBAL    1
#define FSPQ_RELEASE_GLOBAL    2
#define FSPQ_ACQUIRE_INDEX     3
#define FSPQ_RELEASE_INDEX     4

#define FSPQ_GET_STATE        20
#define FSPQ_SET_STATE        21
#define FSPQ_EJECT_ALL        22

#define FSPQ_SHUTDOWN         30

/* Structure returned by Plugin_QueryInfos call */
typedef struct
{
  char *Name;
  char *Version;
  char *Copyright;
  char *Description;
} FSP_TInfos, *FSP_PInfos;

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

void *FS_PluginQuery(int Type,...);

/* Internal prototypes */
FS_PPlugin FS_LoadPlugin(const char Name[]);
void FS_UnLoadPlugin(SU_DL_HANDLE Handle);
void FS_UnLoadAllPlugin(void);
bool FS_ConfigurePlugin(SU_DL_HANDLE Handle);
bool FS_IsPluginValid(FS_PPlugin Plugin);

#ifdef __cplusplus
}
/* Redefine c++ bool type */
#undef bool
#endif /* __cplusplus */

#endif /* !__PLUGIN_H__ */

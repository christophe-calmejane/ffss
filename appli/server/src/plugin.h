#ifndef __PLUGIN_H__
#define __PLUGIN_H__

#include "server.h"

/* Undefine c++ bool type (unsigned char ?)
   Use SU_BOOL type in your appli, every time you use a ffss prototype
 */
#ifdef __cplusplus
#define bool SU_BOOL
#endif /* __cplusplus */

#ifdef _WIN32
#define FSP_BASE_REG_ROOTHKEY HKEY_LOCAL_MACHINE
#define FSP_BASE_REG_ROOTKEY "HKEY_LOCAL_MACHINE"
#define FSP_BASE_REG_SUBKEY "Software\\FFSS\\Server\\PluginsConf\\"
#define FSP_BASE_REG_KEY FSP_BASE_REG_ROOTKEY "\\" FSP_BASE_REG_SUBKEY
#endif /* _WIN32 */

#define FSPQ_ACQUIRE_GLOBAL    1
#define FSPQ_RELEASE_GLOBAL    2
#define FSPQ_ACQUIRE_INDEX     3
#define FSPQ_RELEASE_INDEX     4

#define FSPQ_GET_FILTER_API    10
#define FSPQ_GET_QOS_API    11

#define FSPQ_GET_STATE        20
#define FSPQ_SET_STATE        21
#define FSPQ_EJECT_ALL        22

#define FSPQ_SHUTDOWN         30

#define FSPQ_DBG_FLAGS        40
#define FSPQ_DBG_OUTPUT       41

/* Structure returned by Plugin_QueryInfos call - Freed by Plugin */
typedef struct
{
  char *Name;        /* Freed by Plugin */
  char *Version;     /* Freed by Plugin */
  char *Copyright;   /* Freed by Plugin */
  char *Description; /* Freed by Plugin */
} FSP_TInfos, *FSP_PInfos;

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

void *FS_PluginQuery(int Type,...);

#ifdef __cplusplus
}
/* Redefine c++ bool type */
#undef bool
#endif /* __cplusplus */

#endif /* !__PLUGIN_H__ */

#ifndef __PLUGIN_H__
#define __PLUGIN_H__

#include "server.h"

#define FSPQ_ACQUIRE_GLOBAL    1
#define FSPQ_RELEASE_GLOBAL    2
#define FSPQ_ACQUIRE_INDEX     3
#define FSPQ_RELEASE_INDEX     4
#define FSPQ_LOCK_CONNS        5
#define FSPQ_UNLOCK_CONNS      6
#define FSPQ_LOCK_XFERS        7
#define FSPQ_UNLOCK_XFERS      8

#define FSPQ_GET_STATE        20
#define FSPQ_SET_STATE        21
#define FSPQ_EJECT_ALL        22

void *FS_PluginQuery(int Type,...);



/* Internal prototypes */
FS_PPlugin FS_LoadPlugin(const char Name[]);
void FS_UnLoadPlugin(SU_DL_HANDLE Handle);
void FS_UnLoadAllPlugin(void);
bool FS_ConfigurePlugin(SU_DL_HANDLE Handle);
bool FS_IsPluginValid(FS_PPlugin Plugin);


#endif /* !__PLUGIN_H__ */

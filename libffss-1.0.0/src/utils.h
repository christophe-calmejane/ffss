#ifndef __FFSS_UTILS_H__
#define __FFSS_UTILS_H__

#ifdef __unix__
#include <sys/ioctl.h>
#include <net/if.h>

/* Define for caddr_t structure that differs with unixes */
#ifdef __linux__
#define CADDR_T __caddr_t
#else /* !__linux__ */
#define CADDR_T caddr_t
#endif /* __linux__ */

#endif /* __unix__ */

extern SU_THREAD_HANDLE FFSS_MainThread;
int FFSS_SendBroadcast(SU_PServerInfo SI,char *Text,int len,char *port);
extern SU_PList FFSS_Broadcast; /* char * */

#endif /* !__FFSS_UTILS_H__ */

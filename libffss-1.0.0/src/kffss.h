#ifndef __KFFSS_H__
#define __KFFSS_H__

/* WIN32 DRIVER */
#ifdef _WIN32
#define SU_NO_INCLUDES
#define SU_INCLUDE_NO_SOCKS
#define SU_INCLUDE_NO_REG
#define SU_INCLUDE_NO_THREAD
#define FILE void
#ifndef DWORD
#define DWORD long int
#endif /* !DWORD */
#define strtok_r(a,b,c) strtok(a,b)
#define LOG_INFO    2
#define LOG_WARNING 1
#define LOG_ERR     0
#define time_t unsigned long int
#define sockaddr_in _TRANSPORT_ADDRESS *
#ifndef SU_PClientSocket
#error "You must define SU_PClientSocket to your internal driver type before including ffss.h !!"
#endif /* !SU_PClientSocket */
#define FFSS_PTransfer void *

#else /* !_WIN32 */
#error "Undefined driver type"
#endif /* _WIN32 */

#endif /* !__KFFSS_H__ */

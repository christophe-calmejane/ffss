/*
    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/
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

#ifndef FFSS_DRIVER
extern SU_THREAD_HANDLE FFSS_MainThread;
#endif /* !FFSS_DRIVER */
int FFSS_SendBroadcast(SU_PServerInfo SI, char *Text, size_t len, char *port);
extern SU_PList FFSS_Broadcast; /* char * */

#endif /* !__FFSS_UTILS_H__ */

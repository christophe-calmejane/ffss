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
#ifndef __FFSS_COMMON_H__
#define __FFSS_COMMON_H__

SU_THREAD_ROUTINE(F_ThreadUDP,User);
void FFSS_SignalHandler_BrokenPipe(int sig);
void FFSS_SignalHandler_Term(int sig);

typedef struct
{
  SU_TICKS st;
	FFSS_Field IP;
  FFSS_LongField bytes;
  FFSS_LongField prev_thrpt;
} FFSS_TQosConn,*FFSS_PQosConn;

#define FFSS_MAX_SOCKETS 65536

extern FFSS_PQosConn FFSS_QosConns[FFSS_MAX_SOCKETS];
extern bool FFSS_ShuttingDown;

#endif /* !__FFSS_COMMON_H__ */

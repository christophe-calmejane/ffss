#ifndef __FFSS_COMMON_H__
#define __FFSS_COMMON_H__

SU_THREAD_ROUTINE(F_ThreadUDP,User);
void FFSS_SignalHandler_BrokenPipe(int sig);
void FFSS_SignalHandler_Term(int sig);

typedef struct
{
  SU_TICKS st;
  unsigned long IP;
  unsigned long int bytes;
  unsigned long int prev_thrpt;
} FFSS_TQosConn,*FFSS_PQosConn;

#define FFSS_MAX_SOCKETS 65536

extern FFSS_PQosConn FFSS_QosConns[FFSS_MAX_SOCKETS];
extern bool FFSS_ShuttingDown;

#endif /* !__FFSS_COMMON_H__ */

#ifndef __FFSS_COMMON_H__
#define __FFSS_COMMON_H__

SU_THREAD_ROUTINE(F_ThreadUDP,User);
void FFSS_SignalHandler_BrokenPipe(int sig);
void FFSS_SignalHandler_Term(int sig);

#endif

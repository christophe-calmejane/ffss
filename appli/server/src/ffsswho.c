/*
  gcc ffsswho.c -o ffsswho -O2 -Wall -pthread -I /home/killer/travail/ffss/libffss-1.0.0/skyutils-1.15 -L/home/killer/travail/ffss/libffss-1.0.0/skyutils-1.15 -I/usr/local/include -L/home/killer/travail/ffss/libffss-1.0.0/src/.libs -I/home/killer/travail/ffss/libffss-1.0.0/src /home/killer/travail/ffss/libffss-1.0.0/src/.libs/libffss.a /home/killer/travail/ffss/libffss-1.0.0/src/.libs/libskyutils.a
*/

#include "server.h"

#define FFSSWHO_VERSION "1.0-pre3"

bool RequestConns(SU_PClientSocket Client,const char Path[])
{
  char Buf[10000];
  FFSS_Field Size;
  int Got,Pos,Nb,I,J;
  fd_set rfds;
  struct timeval tv;
  int retval;
  char *IP,*X,*Y,*File,*Pct;

  /* Create request */
  Buf[0] = FS_OPCODE_GETSHRCONNS;
  Size = 1;
  SU_strcpy(Buf+Size,Path,sizeof(Buf)-Size);
  Size += strlen(Path) + 1;
  /* Send request */
  SU_ClientSendBuf(Client,(char *)&Size,sizeof(Size));
  SU_ClientSendBuf(Client,Buf,Size);

  /* Prepare to get answer */
  FD_ZERO(&rfds);
  FD_SET(Client->sock,&rfds);
  tv.tv_sec = 5;
  tv.tv_usec = 0;
  retval = select(Client->sock+1,&rfds,NULL,NULL,&tv);
  if(!retval)
  {
    SU_FreeCS(Client);
    return false;
  }
  Got = recv(Client->sock,&Size,sizeof(Size),SU_MSG_NOSIGNAL);
  if(Got != sizeof(Size))
  {
    SU_FreeCS(Client);
    return false;
  }
  Got = recv(Client->sock,Buf,Size,SU_MSG_NOSIGNAL);
  if(Got < 1)
  {
    SU_FreeCS(Client);
    return false;
  }
  if(Buf[0] != FS_OPCODE_ACK)
    return true;
  while(Got < Size)
  {
    Got += recv(Client->sock,Buf+Got,Size-Got,SU_MSG_NOSIGNAL);
  }
  Pos = 1;
  Nb = atoi(Buf+Pos);
  Pos += strlen(Buf+Pos) + 1;
  if(Nb == 0)
  {
    printf("No connection\n");
    return true;
  }
  printf("%-15s  %-35s  Tranfers  Streamings\n","IP","Host");
  printf("--------------------------------------------------------------------------\n");
  for(I=0;I<Nb;I++)
  {
    IP = Buf + Pos;
    Pos += strlen(IP) + 1;
    X = Buf + Pos;
    Pos += strlen(X) + 1;
    Y = Buf + Pos;
    Pos += strlen(Y) + 1;
    printf("%-15s  %-35s  %-8s  %-3s\n",IP,SU_NameOfPort(IP),X,Y);
    if(atoi(X) != 0)
    {
      printf("  Current tranfers : \n");
      for(J=0;J<atoi(X);J++)
      {
        File = Buf + Pos;
        Pos += strlen(File) + 1;
        Pct = Buf + Pos;
        Pos += strlen(Pct) + 1;
        printf("    %s : %s%%\n",File,Pct);
      }
    }
    if(atoi(Y) != 0)
    {
      printf("  Current streamings : \n");
      for(J=0;J<atoi(Y);J++)
      {
        File = Buf + Pos;
        Pos += strlen(File) + 1;
        Pct = Buf + Pos;
        Pos += strlen(Pct) + 1;
        printf("    %s : %s%%\n",File,Pct);
      }
    }
    if((atoi(X) != 0) || (atoi(Y) != 0))
      printf("\n");
  }
  printf("\n");
  return true;
}

bool RequestSharesList(SU_PClientSocket Client)
{
  char Buf[10000];
  FFSS_Field Size;
  int Got,Pos,Nb,I,Conns,XFers;
  fd_set rfds;
  struct timeval tv;
  int retval;
  char *Shr,*Path;

  /* Create request */
  Buf[0] = FS_OPCODE_GETSHRLIST;
  Size = 1;
  /* Send request */
  SU_ClientSendBuf(Client,(char *)&Size,sizeof(Size));
  SU_ClientSendBuf(Client,Buf,Size);

  /* Prepare to get answer */
  FD_ZERO(&rfds);
  FD_SET(Client->sock,&rfds);
  tv.tv_sec = 5;
  tv.tv_usec = 0;
  retval = select(Client->sock+1,&rfds,NULL,NULL,&tv);
  if(!retval)
  {
    SU_FreeCS(Client);
    return false;
  }
  Got = recv(Client->sock,&Size,sizeof(Size),SU_MSG_NOSIGNAL);
  if(Got != sizeof(Size))
  {
    SU_FreeCS(Client);
    return false;
  }
  Got = recv(Client->sock,Buf,Size,SU_MSG_NOSIGNAL);
  if(Got < 1)
  {
    SU_FreeCS(Client);
    return false;
  }
  if(Buf[0] != FS_OPCODE_ACK)
    return true;
  while(Got < Size)
  {
    Got += recv(Client->sock,Buf+Got,Size-Got,SU_MSG_NOSIGNAL);
  }
  Pos = 1;
  Nb = atoi(Buf+Pos);
  Pos += strlen(Buf+Pos) + 1;
  if(Nb == 0)
    return true;
  for(I=0;I<Nb;I++)
  {
    Shr = Buf + Pos;
    Pos += strlen(Buf+Pos) +1;
    Path = Buf + Pos;
    Pos += strlen(Buf+Pos) +1;
    Pos += strlen(Buf+Pos) +1;
    Conns = atoi(Buf + Pos);
    Pos += strlen(Buf+Pos) +1;
    XFers = atoi(Buf + Pos);
    Pos += strlen(Buf+Pos) +1;
    if((Conns+XFers) != 0)
    {
      printf("Connections for share \"%s\" : \n",Shr);
      if(!RequestConns(Client,Path))
        return false;
    }
  }
  return true;
}

int main(int argc,char *argv[])
{
  SU_PClientSocket Client;
  char *Server;

#ifdef _WIN32
  if(!SU_WSInit(2,2))
  {
    printf("Cannot initialize winsock2 !\n");
    return -1;
  }
#endif
  printf("FFSS Who v%s (c) Ze KiLleR / SkyTech 2001'02\n",FFSSWHO_VERSION);
  printf("FFSS Server v%s (c) Ze KiLleR / SkyTech 2001'02\n\n",FFSS_SERVER_VERSION);

  Server = "localhost";
  Client = SU_ClientConnect(Server,FFSS_SERVER_CONF_PORT_S,SOCK_STREAM);
  if(Client == NULL)
  {
    printf("Cannot connect to %s:%s\n",Server,FFSS_SERVER_CONF_PORT_S);
    return -2;
  }

  if(RequestSharesList(Client) == false)
  {
    printf("Cannot request shares for %s\n",Server);
    return -3;
  }

  SU_FreeCS(Client);
  return 0;
}

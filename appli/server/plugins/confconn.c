/* This is the conf conn plugin for ffss server */
/* (c) Christophe Calmejane - 2001'02           */
/*     aka Ze KiLleR / SkyTech                  */
/*                                              */
/* http://zekiller.skytech.org                  */
/* mailto : zekiller@skytech.org                */

#define CONFCONN_NAME      "Conf Conn Plugin"
#define CONFCONN_VERSION   "0.1"
#define CONFCONN_COPYRIGHT "(c) Ze KiLleR - 2001'02"

/* The only file we need to include is server.h */
#include "../src/server.h"
#undef malloc
#undef strdup

/* We have to declare a FS_PPlugin structure for our callbacks */
FS_PPlugin Pl;

/* We declare a callback for the OnCheckConfConn callback */
bool OnCheckConfConn(SU_PClientSocket Client)
{
  printf("In plugin ConfConn\n");
  return true;
}


/* This is the Init fonction (Name it CAREFULLY) called on each LoadPlugin call */
FS_PLUGIN_EXPORT FS_PPlugin Plugin_Init(void)
{
  /* Setting all callbacks to NULL */
  Pl = (FS_PPlugin) malloc(sizeof(FS_TPlugin));
  if(Pl == NULL)
    return NULL;
  memset(Pl,0,sizeof(FS_TPlugin));

  /* Setting plugin infos */
  Pl->Name = strdup(CONFCONN_NAME);
  Pl->Author = strdup(CONFCONN_COPYRIGHT);
  Pl->Version = strdup(CONFCONN_VERSION);

  /* Setting our callbacks */
  Pl->OnCheckConfConn = OnCheckConfConn;

  /* And finaly returning the FS_PPlugin structure to the server.
   * If something goes wrong during this init function, free everything you have allocated and return NULL.
   * UnInit function will not be called in this case.
  */
  return Pl;
}

/* This is the UnInit fonction (Name it CAREFULLY) called on each UnLoadPlugin call */
FS_PLUGIN_EXPORT void Plugin_UnInit(void)
{
  /* Nothing to do, as Pl->Name is freed after */
}


/*
    This is a ffssfs file system driver for Windows NT/2000/XP.
    Copyright (C) 2002 Christophe Calmejane
    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#include "ntifs.h"
#include "fsd.h"

//
// The following is a subset of linux/lib/string.c from version 2.2.14
//

/*
 *  linux/lib/string.c
 *
 *  Copyright (C) 1991, 1992  Linus Torvalds
 */

size_t strnlen(const char * s, size_t count)
{
    const char *sc;

    for (sc = s; count-- && *sc != '\0'; ++sc)
        /* nothing */;
    return sc - s;
}

/* The following is a subset of skyutils library (string.c) (c) Christophe Calmejane */
unsigned char FFSS_toupper(unsigned char c)
{
  if((c >= 'a') && (c <= 'z'))
    return (c-32);
  if((c >= 224) && (c <= 255))
    return (c-32);
  return c;
}

unsigned char FFSS_tolower(unsigned char c)
{
  if((c >= 'A') && (c <= 'Z'))
    return (c+32);
  if((c >= 192) && (c <= 223))
    return (c+32);
  return c;
}

int FFSS_strcasecmp(const char *s,const char *p)
{
  while((*s != 0) && (*p != 0))
  {
    if(FFSS_toupper(*s) != FFSS_toupper(*p))
      return 0;
    s++;p++;
  }
  return ((*s == 0) && (*p == 0));
}


//
// End of subset of linux/lib/string.c
//

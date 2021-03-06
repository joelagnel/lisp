/*
 * good-mood.h
 * generated by gwizard 0.0.0 on Fri Nov 16 16:06:58 2001
 */

/*
** Copyright (C) 2001 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
**
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation; either version 2 of the License, or
** (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with this program; if not, write to the Free Software
** Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
**
*/

#ifndef __GOOD_MOOD_H__
#define __GOOD_MOOD_H__

#include <bonobo/bonobo-xobject.h>
#include "Bonobo_Sample_Moody.h"

#ifdef __cplusplus
extern "C" {
#endif /*__cplusplus*/

#define GOOD_MOOD_TYPE                (good_mood_get_type ())
#define GOOD_MOOD(obj)                (GTK_CHECK_CAST ((obj), GOOD_MOOD_TYPE, GoodMood))
#define GOOD_MOOD_CLASS(klass)        (GTK_CHECK_CLASS_CAST((klass), GOOD_MOOD_TYPE, GoodMoodClass))
#define IS_GOOD_MOOD(obj)             (GTK_CHECK_TYPE ((obj), GOOD_MOOD_TYPE))
#define IS_GOOD_MOOD_CLASS(klass)     (GTK_CHECK_CLASS_TYPE ((klass), GOOD_MOOD_TYPE))

typedef struct _GoodMood       GoodMood;
typedef struct _GoodMoodClass  GoodMoodClass;

struct _GoodMood {
	BonoboXObject parent;

	/* TODO: add member vars */
};

struct _GoodMoodClass {
	BonoboXObjectClass parent_class;
	POA_Bonobo_Sample_GoodMood__epv epv;

	/* TODO: add other class vars, signals here */
};


GtkType          good_mood_get_type   (void);
GoodMood*        good_mood_new        (void);
CORBA_char*      good_mood_say_hello  (PortableServer_Servant _servant,
				       CORBA_Environment * ev);   


/* TODO: add custom function declarations here */

#ifdef __cplusplus
}
#endif /*__cplusplus*/

#endif /*__GOOD_MOOD_H__*/

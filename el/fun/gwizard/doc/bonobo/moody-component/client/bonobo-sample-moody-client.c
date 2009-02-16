/*
 *  bonobo-sample-moody-client.c
 */

#include <gnome.h>
#include <liboaf/liboaf.h>
#include <bonobo.h>
#include "Bonobo_Sample_Moody.h"


int
main (int argc, char *argv[])
{
	CORBA_ORB orb;
	CORBA_Environment ev;

	BonoboObjectClient *server;

	Bonobo_Unknown moody_object;
	Bonobo_Sample_GoodMood good_mood;
	Bonobo_Sample_BadMood  bad_mood;
	
	gnome_init_with_popt_table ("bonobo-sample-moody-client", 
				    "0.0.0", argc, argv,
				    oaf_popt_options, 0, NULL);
	
	if ((orb = oaf_init (argc, argv)) == CORBA_OBJECT_NIL)
		g_error ("could not init orb\n");
	
	if (!bonobo_init (orb, CORBA_OBJECT_NIL, CORBA_OBJECT_NIL))
		g_error ("could not initialize bonobo\n");

	bonobo_activate ();
	
	if (!(server = bonobo_object_activate (
		      "OAFIID:Bonobo_Sample_MoodyComponent", 0)))
		g_error ("failed to create a moody component\n");
	
	CORBA_exception_init (&ev);
	moody_object = BONOBO_OBJREF (server);

	/*
	 *  good mood
	 */
	good_mood = bonobo_object_client_query_interface 
		(server, "IDL:Bonobo/Sample/GoodMood:1.0", &ev);
	if (BONOBO_EX(&ev))
		g_warning ("error querying interface\n");
	else {
		char *msg = NULL;

		g_print ("Q: hey good mood component, how are you?\n");
		msg = Bonobo_Sample_GoodMood_say_hello (good_mood, &ev);
		if (BONOBO_EX(&ev)) 
			g_warning ("error in say_hello\n");
		else
			g_print ("A: %s\n", msg);
		
		CORBA_exception_free (&ev);
		CORBA_free (msg);
		
		bonobo_object_release_unref (good_mood, NULL); 
	}

	/* 
	 * bad mood 
	 */
	bad_mood = bonobo_object_client_query_interface 
		(server, "IDL:Bonobo/Sample/BadMood:1.0", &ev);
	if (BONOBO_EX(&ev))
		g_warning ("error querying interface\n");
	else {
		char *msg = NULL;

		g_print ("Q: hey bad mood component, how are you?\n");
		msg = Bonobo_Sample_BadMood_say_hi (bad_mood, &ev);
		if (BONOBO_EX(&ev)) 
			g_warning ("error in say_hello\n");
		else
			g_print ("A: %s\n", msg);
		
		CORBA_exception_free (&ev);
		CORBA_free (msg);
		
		bonobo_object_release_unref (bad_mood, NULL);       
	}
	
			
	CORBA_exception_free (&ev);
	bonobo_object_unref (BONOBO_OBJECT (server));
	
	return 0;
}

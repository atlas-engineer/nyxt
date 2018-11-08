//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//

#import "NextApplication.h"

#include <xmlrpc-c/base.h>
#include <xmlrpc-c/client.h>
#include <xmlrpc-c/config.h>

#define NAME "Next"
#define VERSION "0.1"

@implementation NextApplication

static void
reportIfFaultOccurred (xmlrpc_env * const envP) {
    if (envP->fault_occurred) {
        fprintf(stderr, "ERROR: %s (%d)\n",
                envP->fault_string, envP->fault_code);
    }
}

- (void)sendEvent:(NSEvent *)event
{
    if ([event type] == NSEventTypeKeyDown) {
        NSEventModifierFlags modifierFlags = [event modifierFlags];
        char characterCodePressed = [[event charactersIgnoringModifiers] characterAtIndex: 0];
        bool controlPressed = (modifierFlags & NSEventModifierFlagControl);
        bool alternatePressed = (modifierFlags & NSEventModifierFlagOption);
        bool commandPressed = (modifierFlags & NSEventModifierFlagCommand);

        dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_BACKGROUND, 0), ^{
            xmlrpc_env env;
            const char * const serverUrl = "http://localhost:8081/RPC2";
            const char * const methodName = "PUSH-KEY-CHORD";
            
            // Initialize our error-handling environment.
            xmlrpc_env_init(&env);
            
            // Start up our XML-RPC client library.
            xmlrpc_client_init2(&env, XMLRPC_CLIENT_NO_FLAGS, NAME, VERSION, NULL, 0);
            reportIfFaultOccurred(&env);
            
            // Make the remote procedure call
            xmlrpc_client_call(&env, serverUrl, methodName,
                               "(bbbi)",
                               (xmlrpc_bool) controlPressed,
                               (xmlrpc_bool) alternatePressed,
                               (xmlrpc_bool) commandPressed,
                               (xmlrpc_int) characterCodePressed);
            reportIfFaultOccurred(&env);
            xmlrpc_client_cleanup();
        });
        return;
    } else {
        [super sendEvent:event];
    }
}

@end

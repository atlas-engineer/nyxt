//
//  NextApplication.m
//  next-cocoa
//
//  Created by John Mercouris on 3/25/18.
//  Copyright © 2018 Next. All rights reserved.
//

#import "NextApplication.h"

#include <xmlrpc-c/base.h>
#include <xmlrpc-c/client.h>
#include <xmlrpc-c/config.h>

#define NAME "Next"
#define VERSION "0.1"

@implementation NextApplication

static void
dieIfFaultOccurred (xmlrpc_env * const envP) {
    if (envP->fault_occurred) {
        fprintf(stderr, "ERROR: %s (%d)\n",
                envP->fault_string, envP->fault_code);
        exit(1);
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
            xmlrpc_value * resultP;
            xmlrpc_int consumed;
            const char * const serverUrl = "http://localhost:8081/RPC2";
            const char * const methodName = "PUSH-KEY-CHORD";
            
            // Initialize our error-handling environment.
            xmlrpc_env_init(&env);
            
            // Start up our XML-RPC client library.
            xmlrpc_client_init2(&env, XMLRPC_CLIENT_NO_FLAGS, NAME, VERSION, NULL, 0);
            dieIfFaultOccurred(&env);
            
            // Make the remote procedure call
            resultP = xmlrpc_client_call(&env, serverUrl, methodName,
                                         "(bbbi)",
                                         (xmlrpc_bool) controlPressed,
                                         (xmlrpc_bool) alternatePressed,
                                         (xmlrpc_bool) commandPressed,
                                         (xmlrpc_int) characterCodePressed);
            dieIfFaultOccurred(&env);
            
            xmlrpc_read_int(&env, resultP, &consumed);
            dieIfFaultOccurred(&env);
            printf("The sequence was consumed: %d\n", consumed);
            
            // Dispose of our result value.
            xmlrpc_DECREF(resultP);
            xmlrpc_env_clean(&env);
            xmlrpc_client_cleanup();
            
            if (consumed == 0) {
                dispatch_sync(dispatch_get_main_queue(), ^{
                    [super sendEvent:event];
                });
            }
        });
        
    } else {
        [super sendEvent:event];
    }
}

@end

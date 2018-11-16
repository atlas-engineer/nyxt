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
#include "Global.h"

@implementation NextApplication

- (void)sendEvent:(NSEvent *)event
{
    if ([event type] == NSEventTypeKeyDown && [event timestamp] != 0) {
        NSEventModifierFlags modifierFlags = [event modifierFlags];
        char characterCodePressed = [[event charactersIgnoringModifiers] characterAtIndex: 0];
        bool controlPressed = (modifierFlags & NSEventModifierFlagControl);
        bool alternatePressed = (modifierFlags & NSEventModifierFlagOption);
        bool commandPressed = (modifierFlags & NSEventModifierFlagCommand);
        
        dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_BACKGROUND, 0), ^{
            xmlrpc_env env = [[Global sharedInstance] getXMLRPCEnv];
            xmlrpc_value * resultP;
            xmlrpc_int consumed;
            const char * const serverUrl = "http://localhost:8081/RPC2";
            const char * const methodName = "PUSH-KEY-CHORD";
            
            // Make the remote procedure call
            resultP = xmlrpc_client_call(&env, serverUrl, methodName,
                                         "(bbbi)",
                                         (xmlrpc_bool) controlPressed,
                                         (xmlrpc_bool) alternatePressed,
                                         (xmlrpc_bool) commandPressed,
                                         (xmlrpc_int) characterCodePressed);
            
            xmlrpc_read_int(&env, resultP, &consumed);
            if (!consumed) {
                dispatch_sync(dispatch_get_main_queue(), ^{
                    [super sendEvent: [NSEvent keyEventWithType:NSEventTypeKeyDown
                                                       location:[event locationInWindow]
                                                  modifierFlags:[event modifierFlags]
                                                      timestamp:0
                                                   windowNumber:[event windowNumber]
                                                        context:nil
                                                     characters:[event characters]
                                    charactersIgnoringModifiers:[event charactersIgnoringModifiers]
                                                      isARepeat:[event isARepeat]
                                                        keyCode:[event keyCode]]];
                });
            }
        });
    } else {
        [super sendEvent:event];
    }
}

@end

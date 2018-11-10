//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//

#import "Minibuffer.h"

#include <xmlrpc-c/base.h>
#include <xmlrpc-c/client.h>
#include <xmlrpc-c/config.h>
#include "Global.h"


@implementation Minibuffer
@synthesize callBackCount;
@synthesize parentWindowIdentifier;

- (NSString *)stringByEvaluatingJavaScriptFromString:(NSString *)script
{
    [self setCallBackCount:[self callBackCount] + 1];
    [self evaluateJavaScript:script completionHandler:^(id result, NSError *error) {
        if (error == nil) {
            if (result != nil) {
                NSString* transformedResult = [NSString stringWithFormat:@"%@", result];
                // Call XML RPC With Result
                dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_BACKGROUND, 0), ^{
                    xmlrpc_env env = [[Global sharedInstance] getXMLRPCEnv];
                    const char * const serverUrl = "http://localhost:8081/RPC2";
                    const char * const methodName = "MINIBUFFER-JAVASCRIPT-CALL-BACK";
                    
                    // Make the remote procedure call
                    xmlrpc_client_call(&env, serverUrl, methodName,
                                       "(sss)",
                                       [[self parentWindowIdentifier] UTF8String],
                                       [transformedResult UTF8String],
                                       [[@([self callBackCount]) stringValue] UTF8String]);
                });
            }
        } else {
            NSLog(@"evaluateJavaScript error : %@", error.localizedDescription);
        }
    }];
    return [@([self callBackCount]) stringValue];
}

@end

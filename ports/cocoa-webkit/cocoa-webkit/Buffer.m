//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//

#import "Buffer.h"

#include <xmlrpc-c/base.h>
#include <xmlrpc-c/client.h>
#include <xmlrpc-c/config.h>

#define NAME "Next"
#define VERSION "0.1"

@implementation Buffer
@synthesize callBackCount;
@synthesize identifier;

- (instancetype)init
{
    self = [super init];
    [self setTranslatesAutoresizingMaskIntoConstraints:NO];
    [self setURL:@"file:///Users/jmercouris/Downloads/webpage.html"];
    [self setCallBackCount:0];
    return self;
}

- (NSString *)stringByEvaluatingJavaScriptFromString:(NSString *)script
{
    [self setCallBackCount:[self callBackCount] + 1];

    [self evaluateJavaScript:script completionHandler:^(id result, NSError *error) {
        if (error == nil) {
            if (result != nil) {
                NSString* transformedResult = [NSString stringWithFormat:@"%@", result];
                // Call XML RPC With Result
                dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_BACKGROUND, 0), ^{
                    xmlrpc_env env;
                    const char * const serverUrl = "http://localhost:8081/RPC2";
                    const char * const methodName = "JAVASCRIPT-CALL-BACK";
                    
                    // Initialize our error-handling environment.
                    xmlrpc_env_init(&env);
                    
                    // Start up our XML-RPC client library.
                    xmlrpc_client_init2(&env, XMLRPC_CLIENT_NO_FLAGS, NAME, VERSION, NULL, 0);
                    
                    // Make the remote procedure call
                    xmlrpc_client_call(&env, serverUrl, methodName,
                                       "(sss)",
                                       [[self identifier] UTF8String],
                                       [transformedResult UTF8String],
                                       [[@([self callBackCount]) stringValue] UTF8String]);
                    xmlrpc_client_cleanup();
                });
            }
        } else {
            NSLog(@"evaluateJavaScript error : %@", error.localizedDescription);
        }
    }];
    return [@([self callBackCount]) stringValue];
}

- (void)setURL:(NSString *)URL
{
    NSURLRequest *urlRequest = [NSURLRequest requestWithURL:[NSURL URLWithString:URL]];
    [self loadRequest:urlRequest];
}

@end

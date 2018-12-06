//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//

#import "Global.h"

#define NAME "Next"
#define VERSION "0.1"

@implementation Global

+ (Global *)sharedInstance {
    static dispatch_once_t onceToken;
    static Global *instance = nil;
    dispatch_once(&onceToken, ^{
        instance = [[Global alloc] init];
    });
    return instance;
}

static void
reportIfFaultOccurred (xmlrpc_env * const envP) {
    if (envP->fault_occurred) {
        fprintf(stderr, "ERROR: %s (%d)\n",
                envP->fault_string, envP->fault_code);
    }
}

- (id)init {
    self = [super init];
    if (self) {
        dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_HIGH, 0), ^{
            // Initialize our error-handling environment.
            xmlrpc_env_init(&self->env);
            // Start up our XML-RPC client library.
            xmlrpc_client_init2(&self->env, XMLRPC_CLIENT_NO_FLAGS, NAME, VERSION, NULL, 0);
            reportIfFaultOccurred(&self->env);
        });
    }
    return self;
}

- (xmlrpc_env)getXMLRPCEnv {
    return self->env;
}

@end

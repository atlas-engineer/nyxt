//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//

#import <Foundation/Foundation.h>
#import <AppKit/AppKit.h>
#include <xmlrpc-c/base.h>
#include <xmlrpc-c/client.h>
#include <xmlrpc-c/config.h>

@interface Global : NSObject
{
    xmlrpc_env env;
}

+ (Global *)sharedInstance;
- (xmlrpc_env) getXMLRPCEnv;

@end

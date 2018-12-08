//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//

#import "Global.h"

@implementation Global

@synthesize port;
@synthesize coreSocket;

+ (Global *)sharedInstance {
    static dispatch_once_t onceToken;
    static Global *instance = nil;
    dispatch_once(&onceToken, ^{
        instance = [[Global alloc] init];
    });
    return instance;
}

@end

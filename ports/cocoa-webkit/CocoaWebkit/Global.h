//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//

#import <AppKit/AppKit.h>
#import <Foundation/Foundation.h>

@interface Global : NSObject {
}

+ (Global*)sharedInstance;

@property (strong, atomic, readwrite) NSString* port;
@property (strong, atomic, readwrite) NSString* coreSocket;

@end

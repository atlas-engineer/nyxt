//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//

#import <Foundation/Foundation.h>
#import <AppKit/AppKit.h>

@interface Global : NSObject
{
    NSWindow *_window;
}

+ (Global *)sharedInstance;

@property(strong, nonatomic, readwrite) NSWindow *window;

@end

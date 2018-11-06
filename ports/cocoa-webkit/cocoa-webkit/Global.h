//
//  Global.h
//  cocoa-webkit
//
//  Created by John Mercouris on 3/10/18.
//  Copyright © 2018 Atlas Engineer LLC. All rights reserved.
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

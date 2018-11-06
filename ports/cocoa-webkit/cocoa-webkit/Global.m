//
//  Global.m
//  next-cocoa
//
//  Created by John Mercouris on 3/10/18.
//  Copyright © 2018 Next. All rights reserved.
//

#import "Global.h"

@implementation Global

@synthesize window = _window;

+ (Global *)sharedInstance {
    static dispatch_once_t onceToken;
    static Global *instance = nil;
    dispatch_once(&onceToken, ^{
        instance = [[Global alloc] init];
    });
    return instance;
}

- (id)init {
    self = [super init];
    if (self) {
        // Initialization of variables
    }
    return self;
}

@end

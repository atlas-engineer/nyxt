//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//  

#import "XMLRPCDefaultRequestDecoder.h"

@implementation XMLRPCDefaultRequestDecoder

- (id)init {
    if (self = [super init]) {
        myMethod = [[NSString alloc] init];
        myParameters = [[NSArray alloc] init];
    }
    
    return self;
}

- (id)decodeWithData:(NSData*) data {
    
    // Populate myMethod and myParameters here
    return @"Lol";
}

- (NSString *)method {
    return myMethod;
}

- (NSArray *)parameters {
    return myParameters;
}

@end

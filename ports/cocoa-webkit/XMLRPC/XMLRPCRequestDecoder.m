//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//  

#import "XMLRPCRequestDecoder.h"
#import "XMLRPCDefaultRequestDecoder.h"

@implementation XMLRPCRequestDecoder

- (id)initWithData: (NSData *)data withDecoder: (id<XMLRPCDecoder>)decoder {
    self = [super init];
    if (self) {
        myXMLDecoder = decoder;
        [decoder decodeWithData:data];
    }
    return self;
}

- (id)initWithData: (NSData *)data {
    return [self initWithData:data withDecoder:[[XMLRPCDefaultRequestDecoder alloc] init]];
}

- (NSString *)method {
    return [myXMLDecoder method];
}

- (NSArray *)parameters {
    return [myXMLDecoder parameters];
}

@end

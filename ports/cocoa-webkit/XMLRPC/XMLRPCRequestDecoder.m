//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//  

#import "XMLRPCRequestDecoder.h"

@implementation XMLRPCRequestDecoder

- (id)initWithData: (NSData *)data {
    
    return nil;
}

- (id)initWithData: (NSData *)data withDecoder: (id<XMLRPCDecoder>)decoder {
    myXMLDecoder = decoder;
    
    return nil;
}

- (NSString *)method {
    return [myXMLDecoder method];
}

- (NSArray *)parameters {
    return [myXMLDecoder parameters];
}

@end

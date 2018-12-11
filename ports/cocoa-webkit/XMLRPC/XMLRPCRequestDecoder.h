//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//

#import "XMLRPCDecoder.h"
#import <Foundation/Foundation.h>

@interface XMLRPCRequestDecoder : NSObject {
    id<XMLRPCDecoder> myXMLDecoder;
}

- (id)initWithData:(NSData*)data withDecoder:(id<XMLRPCDecoder>)decoder;

- (id)initWithData:(NSData*)data;

- (NSString*)method;

- (NSArray*)parameters;

@end

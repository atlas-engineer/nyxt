//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//  

#import <Foundation/Foundation.h>
#import "XMLRPCDecoder.h"

@interface XMLRPCRequestDecoder : NSObject {
    id<XMLRPCDecoder> myXMLDecoder;
}

- (id)initWithData: (NSData *)data;

- (NSString *)method;

- (NSArray*)parameters;

@end


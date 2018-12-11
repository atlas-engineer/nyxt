//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//  

#import <Foundation/Foundation.h>

typedef enum {
    XMLRPCElementTypeMethodName,
    XMLRPCElementTypeArray,
    XMLRPCElementTypeDictionary,
    XMLRPCElementTypeMember,
    XMLRPCElementTypeName,
    XMLRPCElementTypeInteger,
    XMLRPCElementTypeDouble,
    XMLRPCElementTypeBoolean,
    XMLRPCElementTypeString,
    XMLRPCElementTypeDate,
    XMLRPCElementTypeData
} XMLRPCElementType;

@protocol XMLRPCDecoder <NSObject>

- (void)decodeWithData:(NSData*) data;

- (NSString *)method;

- (NSArray *)parameters;

@end
